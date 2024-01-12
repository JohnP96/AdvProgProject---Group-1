using System;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using InterpreterFSharp;
using Microsoft.FSharp.Collections;
using System.Diagnostics;

using stringValPair = System.Tuple<string, InterpreterFSharp.LexerParser.Number>;
using terminalList = Microsoft.FSharp.Collections.FSharpList<InterpreterFSharp.LexerParser.terminal>;
using pNeReturnVal = System.Tuple<bool, System.Tuple<Microsoft.FSharp.Collections.FSharpList<InterpreterFSharp.LexerParser.terminal>, System.Tuple<string, InterpreterFSharp.LexerParser.Number>>>;

namespace InterpreterWPF
{

    public partial class MainWindow : Window
    {
        // Graph Variables
        private double zoomLevel = 1;   // The zoom of the graph; resets when a limit is reached 
        private double x_Offset = 0;    // The offset of the graph in the x direcrtion 
        private double y_Offset = 0;    // The offset of the graph in the y direcrtion 

        private double baseInterval = 10; // Initial interval between grey grid lines
        private double baseDarkInterval = 50; // Initial interval between dark grid lines

        private double zoomNum = 2;     // Number of times the graph has been reset due to zooming;

        private Graph testGraph;

        FSharpList<stringValPair> symList;
        terminalList plotTokens;
        terminalList derivative;
        terminalList integral;
        LexerParser.Number start_;
        LexerParser.Number stop_;

        public MainWindow()
        {
            InitializeComponent();
            symList = FSharpList<stringValPair>.Empty;
            //plotTokens = LexerParser.initPlotTokens;
            testGraph = new Graph(graphCanvas, zoomLevel, x_Offset, y_Offset, baseInterval, baseDarkInterval, zoomNum);

        }

        private void enterBtn_Click(object sender, RoutedEventArgs e)
        {
            bool success = true;
            // Print to the screen the thing the user typed
            cmdWindow.AppendText("> " + Input.Text + "\n");

            // Clear the input field for the next command 
            string input = Input.Text.Replace(" ", string.Empty);

            // Call the lexer on the input and return a tokenList of the input
            terminalList lexed = LexerParser.lexer(input);

            // Add the token "Mul! btw the token "Num" and "Vid" i.e 2x -> 2*x
            lexed = LexerParser.insertMulBetweenNumAndVid(lexed);

            // look through the lexed list for the token "Err" and throw err if any is found 
            for (int i = 0; i < lexed.Length; i++)
            {
                if (lexed[i] is LexerParser.terminal.Err)
                {
                    success = false;
                    cmdWindow.AppendText("> Error: " + lexed[i] + " is not a valid lexeme\n");
                }
            }

            // Call the parser on the Lexed input
            string parseRes = LexerParser.parser(lexed, symList).Item1.ToString();

            //cmdWindow.AppendText("> Parser result: " + parseRes + "\n // Testing
            if (parseRes.StartsWith("F"))
            {
                success = false;
                // Print error message
                cmdWindow.AppendText(string.Concat("> ", parseRes.AsSpan(9, (parseRes.Length - 10)), "\n")); // The span gets rid of the success/failure notation and the quotation marks
            }
            else if (parseRes.Substring(9) != "]")
            {
                success = false;
                cmdWindow.AppendText("> Invalid expression.\n");
            }
            if (success)
            {
                // Show the lexed tokens on the screen
                cmdWindow.AppendText("> Tokens: " + string.Join(", ", lexed) + "\n");

                // Result => ((plot, (Integration, (start, stop)), ([tList], (vName, value))), [symList]).... i.e -> ((True,([Lpar; Num (Int 2); Mul; ... ], ("", Int 0)), [])
                var result = LexerParser.parseNevalNsym(lexed, symList);
                bool plot_ = result.Item1.Item1; 
                bool integration_ = result.Item1.Item2.Item1;
                start_ = result.Item1.Item2.Item2.Item1;
                stop_ = result.Item1.Item2.Item2.Item2;
                

                // "answer" -> value
                LexerParser.Number answer = result.Item1.Item3.Item2.Item2;

                // "symList" -> SymList
                symList = result.Item2;

                // "result.Item1.Item1" -> plot
                if (plot_)
                {
                    plotTokens = result.Item1.Item3.Item1;
                    derivative = LexerParser.findDerivative(plotTokens);
                    
                    String derivativeString = LexerParser.tokenToString(LexerParser.simplifyTokens(derivative));

                    // Write the info to the card 
                    Info_derivative.Text = "Derivative: " + derivativeString + "\n";

                    // Draw the graph
                    DrawGraph2(sender, e);
                }
                else if (integration_)
                {
                    // Get the function f(x)
                    plotTokens = result.Item1.Item3.Item1;

                    // Integrate the function
                    integral = LexerParser.findIntegral(plotTokens);

                    // Simplify the Integral
                    String integralString = LexerParser.tokenToString(LexerParser.simplifyTokens(integral));

                    DrawGraph2(sender, e);

                }
                else
                {
                    cmdWindow.AppendText("> Result: " + answer + "\n");
                    //cmdWindow.AppendText("> Sym: " + symList + "\n"); // Testing
                }
                cmdWindow.ScrollToEnd();
            }
            string vars = "\n";
            foreach (stringValPair var in symList)
            {
                vars = vars + var.Item1 + " = " + var.Item2 + "\n";
            }
            VariableTracker.Text = "Variables:" + vars;
            Input.Clear();
        }



        private void DrawGraph2(object sender, RoutedEventArgs e)
        {
            // Clear everything on the canvas
            testGraph.clear(graphCanvas);

            double darkInterval = baseDarkInterval * zoomLevel;
            double interval = baseInterval * zoomLevel;

            // Draw Axis
            testGraph.drawAxis(graphCanvas, x_Offset, y_Offset, zoomLevel);

            // Draw grid lines
            List<double> remainder = testGraph.drawGridLines(graphCanvas, ref interval, baseInterval, darkInterval, x_Offset, y_Offset, ref zoomLevel, ref zoomNum);

            // Draw Labels
            (double increment, List<double> minLabels) = testGraph.drawLabels(graphCanvas, x_Offset, y_Offset, zoomLevel, zoomNum);

            // calculate minX and maxX of Graph
            List<double> resi = CalculateMinAndMax(remainder, increment, minLabels);

            double step = 0.1;
            double scaleFactor = Math.Abs(baseInterval * zoomLevel);

            if (integral != null)
            {
                PlotGraph(resi, step, scaleFactor);
                PlotIntegral(resi, step, scaleFactor);

                // Shade the area under the integral curve
                ShadeAreaUnderGraph(plotTokens, resi, scaleFactor);
                
            }
            else if (plotTokens != null)
            {

                PlotGraph(resi, step, scaleFactor);
                PlotDerivative(resi, step, scaleFactor);

                // Find roots of Polynomial
                double maxIteration = 1000;
                FSharpList<double> roots = LexerParser.newtonMethod(plotTokens, derivative, ListModule.OfSeq(resi), maxIteration, 0.001);

                // Mark roots on the graph
                MarkRoots(roots, scaleFactor);

                // Print roots to Info card
                Info_roots.Text = "Roots: " + roots + "\n";
            }
        }

        private void PlotGraph(List<double> resi, double step, double scaleFactor)
        {
            List<Point> points = new List<Point>();

            // Plot Graph
            if (LexerParser.getNumeric(start_) == 0 && LexerParser.getNumeric(stop_) == 0)
            {
                points = GeneratePoints(resi[1], resi[0], step, plotTokens);
            }
            else
            {
                points = GeneratePoints(LexerParser.getNumeric(start_), LexerParser.getNumeric(stop_), step, plotTokens);
            }

            points = MapPointsToCanvas(points, scaleFactor);
            testGraph.DrawPoints(graphCanvas, points, "Blue");
        }

        private void PlotDerivative(List<double> resi, double step, double scaleFactor)
        {
            // Plot Derivative
            List<Point> points = GeneratePoints(resi[1], resi[0], step, derivative);
            points = MapPointsToCanvas(points, scaleFactor);
            testGraph.DrawPoints(graphCanvas, points, "Red");
        }

        private void MarkRoots(FSharpList<double> roots, double scaleFactor)
        {
            // Mark roots on the graph
            foreach (var root in roots)
            {
                if (root != double.NegativeInfinity && root != double.PositiveInfinity)
                {
                    // Add dots to the Canvas
                    Point p = new Point(root, 0);
                    List<Point> dots = new List<Point>();
                    dots.Add(p);
                    var dot = MapPointsToCanvas(dots, scaleFactor);
                    testGraph.DrawDot(graphCanvas, dot);
                }
            }
            
        }

        private void PlotIntegral(List<double> resi, double step, double scaleFactor)
        {
            // Plot Integral
            List<Point> points = new List<Point>();

            if (LexerParser.getNumeric(start_) == 0 && LexerParser.getNumeric(stop_) == 0)
            {
                points = GeneratePoints(resi[1], resi[0], step, integral);
                // Print roots to Info card
                Info_roots.Text = "Area: " + "\n";
            }
            else
            {
                points = GeneratePoints(LexerParser.getNumeric(start_), LexerParser.getNumeric(stop_), step, integral);
                // Print Area to Info card
                Info_roots.Text = "Area: " + calcArea(integral, LexerParser.getNumeric(start_), LexerParser.getNumeric(stop_)) + "\n";
            }
            // Print Integral string to the info card 
            String integralString = LexerParser.tokenToString(LexerParser.simplifyTokens(integral));
            Info_derivative.Text = "Integral: " + integralString + "\n";

            points = MapPointsToCanvas(points, scaleFactor);
            testGraph.DrawPoints(graphCanvas, points, "Purple");
        }

        private double calcArea(terminalList func, double start, double stop)
        {

            // Remove the string "Float" or "Int" using regular expression and convert to double; more detailed in func GenPoints
            double a1= Convert.ToDouble(Regex.Replace(LexerParser.evalPoly(func, start).ToString(), @"\b(Float|Int)\b", ""));
            double a2= Convert.ToDouble(Regex.Replace(LexerParser.evalPoly(func, stop).ToString(), @"\b(Float|Int)\b", ""));

            return Math.Abs(a2-a1);
        }

        private void ShadeAreaUnderGraph(terminalList func, List<double> resi, double scaleFactor)
        {
            List<Point> points = new List<Point>();
            if (LexerParser.getNumeric(start_) == 0 && LexerParser.getNumeric(stop_) == 0)
            {
                points = GeneratePoints(resi[1], resi[0], 0.1, func);
            }
            else
            {
                points = GeneratePoints(LexerParser.getNumeric(start_), LexerParser.getNumeric(stop_), 0.1, func);
            }
            points = MapPointsToCanvas(points, scaleFactor);

            PathFigure pathFigure = new PathFigure();
            pathFigure.StartPoint = new Point(points.First().X, ((graphCanvas.ActualHeight / 2) + y_Offset) * zoomLevel);

            foreach (var point in points)
            {
                pathFigure.Segments.Add(new LineSegment(point, true));
            }

            // Close the path with a line to the x-axis
            pathFigure.Segments.Add(new LineSegment(new Point(points.Last().X, ((graphCanvas.ActualHeight / 2) + y_Offset) * zoomLevel), true));

            PathGeometry pathGeometry = new PathGeometry();
            pathGeometry.Figures.Add(pathFigure);

            // Create a semi-transparent brush for shading
            SolidColorBrush shadingBrush = new SolidColorBrush(Colors.Blue);
            shadingBrush.Opacity = 0.3;

            // Create a filled polygon shape
            System.Windows.Shapes.Path filledPath = new System.Windows.Shapes.Path();
            filledPath.Fill = shadingBrush;
            filledPath.Data = pathGeometry;

            // Add the filled polygon to the canvas
            graphCanvas.Children.Add(filledPath);
        }


        private List<double> CalculateMinAndMax(List<double> remainder, double increment, List<double> minLabels)
        {
            List<Point> p = new List<Point>();
            for (int i = 0; i < minLabels.Count; i++)
            {
                minLabels[i] += (increment / (baseDarkInterval / baseInterval)) * remainder[i];
            }

            return minLabels;
        }

        private List<Point> GeneratePoints(double minX, double maxX, double step, terminalList func)
        {
            List<Point> points = new List<Point>();

            for (double x = minX; x <= maxX; x += step)
            {
                String res = LexerParser.evalPoly(func, x).ToString();

                // Remove the string "Float" or "Int" using regular expression
                res = Regex.Replace(res, @"\b(Float|Int)\b", "");

                double y = Convert.ToDouble(res);

                points.Add(new Point(x, y));
            }

            return points;
        }

        private double EvaluatePolynomial(List<double> coefficients, double x)
        {
            double result = 0;
            for (int i = 0; i < coefficients.Count; i++)
            {
                result += coefficients[i] * Math.Pow(x, coefficients.Count - 1 - i);
            }
            return result;
        }

        private List<Point> MapPointsToCanvas(List<Point> points, double canvasUnit)
        {
            List<Point> mappedPoints = new List<Point>();

            foreach (Point point in points)
            {
                double canvasX = MapXToCanvas(point.X, canvasUnit);
                double canvasY = MapYToCanvas(point.Y, canvasUnit);

                mappedPoints.Add(new Point(canvasX, canvasY));
            }

            return mappedPoints;
        }


        private double MapXToCanvas(double x, double ratio)
        {
            double d = (baseInterval * x) / (0.5 * Math.Pow(2, zoomNum - 1) / 5);
            d += graphCanvas.ActualWidth / 2;
            d += x_Offset;

            return d * zoomLevel;
        }

        private double MapYToCanvas(double y, double ratio)
        {
            double d = (baseInterval * y) / (0.5 * Math.Pow(2, zoomNum - 1) / 5);
            double res = graphCanvas.ActualHeight / 2 - d;
            res += y_Offset;
            return res * zoomLevel;
        }



        // Panning
        private Point panStartPoint;
        private Point panLastPoint;


        private void mouseDown(object sender, MouseButtonEventArgs e)
        {
            if (e.ChangedButton == MouseButton.Left)
            {
                panStartPoint = e.GetPosition(graphCanvas);
                panLastPoint = panStartPoint;
            }
        }

        private void graphCanvas_MouseMove(object sender, MouseEventArgs e)
        {
            if (e.LeftButton == MouseButtonState.Pressed)
            {
                Point currentMousePosition = e.GetPosition(graphCanvas);

                double deltaX = currentMousePosition.X - panLastPoint.X;
                double deltaY = currentMousePosition.Y - panLastPoint.Y;

                // Adjust the canvas transformation matrix to pan the graph
                x_Offset += deltaX;
                y_Offset += deltaY;

                // Redraw the graph with the new pan offset
                DrawGraph2(sender, e);

                // Update the last point for the next movement
                panLastPoint = currentMousePosition;
            }
        }

        // Zooming
        private void zoom(object sender, MouseWheelEventArgs e)
        {
            // Get the position of the cursor relative to the canvas
            Point cursorPosition = e.GetPosition(graphCanvas);

            // Adjust zoom level based on mouse wheel delta
            if (e.Delta > 0)
                zoomLevel *= 1.04; // Zoom in
            else
                zoomLevel /= 1.04; // Zoom out

            // Redraw the graph with the new zoom level and pan offsets
            DrawGraph2(sender, e);
        }
    }

}
