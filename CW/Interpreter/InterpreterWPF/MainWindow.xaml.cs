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
        terminalList derivativeSym;

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
            cmdWindow.AppendText("> " + Input.Text + "\n");
            string input = Input.Text.Replace(" ", string.Empty);
            terminalList lexed = LexerParser.lexer(input);
            lexed = LexerParser.insertMulBetweenNumAndVid(lexed);
            for (int i = 0; i < lexed.Length; i++){
                if (lexed[i] is LexerParser.terminal.Err)
                {
                    success = false;
                    cmdWindow.AppendText("> Error: " + lexed[i] + " is not a valid lexeme\n");
                }
            }
            string  parseRes = LexerParser.parser(lexed, symList).Item1.ToString();
            //cmdWindow.AppendText("> Parser result: " + parseRes + "\n // Testing
            if (parseRes.StartsWith("F"))
            {
                success = false;
                // Print error message
                cmdWindow.AppendText(string.Concat("> ", parseRes.AsSpan(9,(parseRes.Length-10)), "\n")); // The span gets rid of the success/failure notation and the quotation marks
            }
            else if (parseRes.Substring(9) != "]")
            {
                success = false;
                cmdWindow.AppendText("> Invalid expression.\n");
            }
            if (success)
            {
                cmdWindow.AppendText("> Tokens: " + string.Join(", ", lexed) + "\n");
                Tuple<pNeReturnVal, FSharpList<stringValPair>> result =
                    LexerParser.parseNevalNsym(lexed, symList);
                LexerParser.Number answer = result.Item1.Item2.Item2.Item2;
                symList = result.Item2;

                if (result.Item1.Item1)
                {
                    plotTokens = result.Item1.Item2.Item1;
                    derivative = LexerParser.findDerivative(plotTokens);
                    integral = LexerParser.findIntegral(plotTokens);
                    //(terminalList tester, (String gg, LexerParser.Number ff)) = LexerParser.simplifyTokens(derivative);
                    String derivativeString = LexerParser.tokenToString(LexerParser.simplifyTokens(derivative));
                    String integralString = LexerParser.tokenToString(LexerParser.simplifyTokens(integral));

                    // Write the info to the card 
                    Info_derivative.Text="dy/dx: " + derivativeString + "\n";


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
            List<double> remainder = testGraph.drawGridLines(graphCanvas, ref interval, baseInterval,darkInterval, x_Offset, y_Offset, ref zoomLevel, ref zoomNum);
            
            // Draw Labels
            (double increment, List<double> minLabels) = testGraph.drawLabels(graphCanvas, x_Offset,y_Offset,zoomLevel, zoomNum); // Draws the labels and returns the value of each black line and the last label

            // calculate minX and maxX of Graph 
            List<double> resi = CalculateMinAndMax(remainder, increment, minLabels);

            double step = 0.1;
            double scaleFactor = Math.Abs(baseInterval * zoomLevel);

            // Generate Polynomial data
            //List<double> coefficients = new List<double> { 1, 1}; // Represents x^2 + x

            if (plotTokens != null) {
                // Plot Graph
                List<Point> points = GeneratePoints(resi[1], resi[0], step, plotTokens);
                points = MapPointsToCanvas(points, scaleFactor);
                testGraph.DrawPoints(graphCanvas, points, "Blue");

                // Plot Derivative
                points = GeneratePoints(resi[1], resi[0], step, derivative);
                points = MapPointsToCanvas(points, scaleFactor);
                testGraph.DrawPoints(graphCanvas, points, "Red");

                //// plot Integral
                //points = GeneratePoints(resi[1], resi[0], step, integral);
                //points = MapPointsToCanvas(points, scaleFactor);
                //testGraph.DrawPoints(graphCanvas, points, "Pink");

                // Find roots of Polynomial
                double maxIteration = 1000;

                (bool foundRoot, double staringGuess) = LexerParser.bisectionMethod(plotTokens, resi[1], resi[0]);

                FSharpList<double> roots = LexerParser.newtonMethod(plotTokens, derivative, ListModule.OfSeq(resi), maxIteration, 0.001);


                foreach (var root in roots){
                    if (root != double.NegativeInfinity && root != double.PositiveInfinity)
                    {
                        // Print roots to Info card
                        Info_roots.Text = "Roots: " + root.ToString("F2") + "\n";

                        // Mark roots on the graph
                        Point p = new Point(root, 0);
                        List<Point> dots = new List<Point>();
                        dots.Add(p);
                        var dot = MapPointsToCanvas(dots, scaleFactor);
                        testGraph.DrawDot(graphCanvas, dot);
                    }
                }
            }
        }

        //private void DrawGraph(object sender, RoutedEventArgs e)
        //{
        //    // Clear the Grid lines 
        //    graphCanvas.Children.Clear();

        //    // Draw grid lines
        //    double darkInterval = baseDarkInterval * zoomLevel;
        //    double interval = baseInterval * zoomLevel;
        //    List<double> remainder = DrawGridLines(interval, darkInterval); //  Draws the grid and Calculates how many grey lines after the last black line

        //    // Draw axes
        //    DrawAxis();

        //    // Draw Labels
        //    (double increment, List<double> minLabels) = DrawLabels(); // Draws the labels and returns the value of each black line and the last label

        //    // Generate Polynomial data
        //    //List<double> coefficients = new List<double> { 1, 1}; // Represents x^2 + x


        //    // calculate minX and maxX
        //    List<double> resi = CalculateMinAndMax(remainder, increment, minLabels);
        //    //double minX = resi[1];
        //    //int maxX = resi[0]+1;
        //    double step = 0.1;
        //    double scaleFactor = Math.Abs(baseInterval * zoomLevel);

        //    List<Point> points = GeneratePoints(resi[1], resi[0], step);


        //    points = MapPointsToCanvas(points, scaleFactor);

        //    DrawPoints(points);
        //}

        
        private List<double> CalculateMinAndMax(List<double> remainder, double increment, List<double> minLabels)
        {
            List<Point> p = new List<Point>();
            for (int i = 0; i < minLabels.Count; i++)
            {
                minLabels[i] += (increment/(baseDarkInterval/baseInterval)) * remainder[i];
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

                // Use regular expression to extract numeric part
                //res = Regex.Match(res, @"[-+]?\d+(\.\d+)?").Value;
                //res = res.Substring(6);
                //cmdWindow.AppendText(res.ToString());// Testing

                double y = Convert.ToDouble(res);
                //double y = x + 1;

                points.Add(new Point(x,y));
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

            return d*zoomLevel;
        }

        private double MapYToCanvas(double y, double ratio)
        {
            double d = (baseInterval * y) / (0.5 * Math.Pow(2, zoomNum - 1) / 5);
            double res =  graphCanvas.ActualHeight / 2 - d;
            res += y_Offset;
            return  res*zoomLevel; 
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
