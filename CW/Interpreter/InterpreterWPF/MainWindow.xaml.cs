using System;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.Linq;
using System.Text;
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

namespace InterpreterWPF
{
    
    public partial class MainWindow : Window
    {
        FSharpList<stringValPair> symList;

        public MainWindow()
        {
            InitializeComponent();
            symList = FSharpList<stringValPair>.Empty;
        }

        private void enterBtn_Click(object sender, RoutedEventArgs e)
        {
            bool success = true;
            cmdWindow.AppendText("> " + Input.Text + "\n");
            string input = Input.Text;
            terminalList lexed = LexerParser.lexer(input);
            for (int i = 0; i < lexed.Length; i++){
                if (lexed[i] is LexerParser.terminal.Err)
                {
                    success = false;
                    cmdWindow.AppendText("> Error: " + lexed[i] + " is not a valid lexeme");
                }
            }
            string  parseRes = LexerParser.parser(lexed).ToString();
            //cmdWindow.AppendText("> Parser result: " + parseRes + "\n // Testing
            if (parseRes.StartsWith("F"))
            {
                success = false;
                cmdWindow.AppendText(string.Concat("> ", parseRes.AsSpan(8)));
            }
            else if (parseRes.Substring(9) != "]")
            {
                success = false;
                cmdWindow.AppendText("> Invalid expression.\n");
            }
            if (success)
            {
                cmdWindow.AppendText("> Tokens: " + string.Join(", ", lexed) + "\n");
                Tuple<Tuple<terminalList, stringValPair>, FSharpList<stringValPair>> result =
                    LexerParser.parseNevalNsym(lexed, symList);
                LexerParser.Number answer = result.Item1.Item2.Item2;
                symList = result.Item2;
                cmdWindow.AppendText("> Result: " + answer + "\n");
                cmdWindow.AppendText("> Sym: " + symList + "\n");
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

         // Graph Variables
        private double zoomLevel = 1;
        private double x_Offset = 0;
        private double y_Offset = 0;

        private double baseInterval = 10; // Initial interval between grey grid lines
        private double baseDarkInterval = 50; // Initial interval between dark grid lines

        private double zoomNum = 2;


        private void DrawGraph(object sender, RoutedEventArgs e)
        {
            // Clear the Grid lines 
            graphCanvas.Children.Clear();

            // Draw grid lines
            double darkInterval = baseDarkInterval * zoomLevel;
            double interval = baseInterval * zoomLevel;
            List<double> remainder = DrawGridLines(interval, darkInterval); //  Draws the grid and Calculates how many grey lines after the last black line
            
            // Draw axes
            DrawAxis();

            // Draw Labels
            (double increment, List<double> minLabels) = DrawLabels(); // Draws the labels and returns the value of each black line and the last label

            // Generate Polynomial data
            List<double> coefficients = new List<double> {1, 1}; // Represents x^2 - 3x + 2


            // calculate minX and maxX
            List<double> resi = CalculateMinAndMax(remainder, increment, minLabels);
            double minX = resi[1];
            double maxX = resi[0];
            double step = 1;
            double scaleFactor = Math.Abs(baseInterval * zoomLevel);

            List<Point> points  = GeneratePoly(coefficients, minX, maxX, step);

            points = MapPointsToCanvas(points, scaleFactor);

            DrawPoints(points);
        }

        /*  
            Func:-      DrawGridLines()

            Params:-    interval(double): pixel value between each grey line
                        darkinterval(double): pixel value between each black line

            Return:-    x(list):    List to keep track of how many grey lines have been made after a black line;
                                    this is to find the minimum and maximum number the grid displays. If there
                                    is 2 grey lines after the last black line, find the value of 1 grey line and multiply
                                    it by x(2) + black line label to find the min/max number displayed.
                                    [+x, -x, +y,-y]
         */

        private List<double> DrawGridLines(double interval, double darkInterval)
        {
            List<double> greyLines = new List<double> { 0, 0, 0, 0 }; // Keep track of how many grey lines after a black line
            List<double> blackLines = new List<double> { 0, 0, 0, 0 }; // Keep track of the number of black lines

            double halfWidth = ((graphCanvas.ActualWidth / 2) + x_Offset) * zoomLevel;
            double halfHeight = ((graphCanvas.ActualHeight / 2) + y_Offset) * zoomLevel;

            // Check if the Grid needs to be reset
            CheckZoomReset(interval);

            // Draw light gray grid lines
            for (double x = halfWidth; x <= graphCanvas.ActualWidth; x += interval) // +ve X-axis
            {
                Line line = new Line
                {
                    X1 = x,
                    Y1 = 0,
                    X2 = x,
                    Y2 = graphCanvas.ActualHeight,
                    Stroke = Brushes.LightGray
                };
                graphCanvas.Children.Add(line);
                greyLines[0]++;
            }

            for (double x = halfWidth; x >= 0; x -= interval)
            {
                Line line = new Line
                {
                    X1 = x,
                    Y1 = 0,
                    X2 = x,
                    Y2 = graphCanvas.ActualHeight,
                    Stroke = Brushes.LightGray
                };
                graphCanvas.Children.Add(line);
                greyLines[1]++;
            }

            for (double y = halfHeight; y <= graphCanvas.ActualHeight; y += interval)
            {
                Line line = new Line
                {
                    X1 = 0,
                    Y1 = y,
                    X2 = graphCanvas.ActualWidth,
                    Y2 = y,
                    Stroke = Brushes.LightGray
                };
                graphCanvas.Children.Add(line);
                greyLines[2]++;
            }

            for (double y = halfHeight; y >= 0; y -= interval)
            {
                Line line = new Line
                {
                    X1 = 0,
                    Y1 = y,
                    X2 = graphCanvas.ActualWidth,
                    Y2 = y,
                    Stroke = Brushes.LightGray
                };
                graphCanvas.Children.Add(line);
                greyLines[3]++;
            }

            // Draw dark gray grid lines with a larger interval
            for (double x = halfWidth; x <= graphCanvas.ActualWidth; x += darkInterval)
            {
                Line line = new Line
                {
                    X1 = x,
                    Y1 = 0,
                    X2 = x,
                    Y2 = graphCanvas.ActualHeight,
                    Stroke = Brushes.Black
                };
                graphCanvas.Children.Add(line);
                blackLines[0]++;
            }

            for (double x = halfWidth; x >= 0; x -= darkInterval)
            {
                Line line = new Line
                {
                    X1 = x,
                    Y1 = 0,
                    X2 = x,
                    Y2 = graphCanvas.ActualHeight,
                    Stroke = Brushes.Black
                };
                graphCanvas.Children.Add(line);
                blackLines[1]++;
            }

            for (double y = halfHeight; y <= graphCanvas.ActualHeight; y += darkInterval)
            {
                Line line = new Line
                {
                    X1 = 0,
                    Y1 = y,
                    X2 = graphCanvas.ActualWidth,
                    Y2 = y,
                    Stroke = Brushes.Black
                };
                graphCanvas.Children.Add(line);
                blackLines[2]++;
            }

            for (double y = halfHeight; y >= 0; y -= darkInterval)
            {
                Line line = new Line
                {
                    X1 = 0,
                    Y1 = y,
                    X2 = graphCanvas.ActualWidth,
                    Y2 = y,
                    Stroke = Brushes.Black
                };
                graphCanvas.Children.Add(line);
                blackLines[3]++;
            }

            // Return the result
            List<double> result = new List<double> {
                greyLines[0] % blackLines[0],
                greyLines[1] % blackLines[1],
                greyLines[2] % blackLines[2],
                greyLines[3] % blackLines[3]
            };

            return result;
        }

        private List<double> CalculateMinAndMax(List<double> remainder, double increment, List<double> minLabels)
        {
            for (int i = 0; i < minLabels.Count; i++)
            {
                minLabels[i] += (increment/(baseDarkInterval/baseInterval)) * remainder[i];
            }

            return minLabels;
        }


        /*
            Func:-      DrawLabels()

            Return:-    result(Tuple):  [0] - The value of each black line
                                        [1] - List of The last label drawn in each sector [+x,-x,+y,-y]
         */
        private (double, List<double>) DrawLabels()
        {
            double halfWidth = (graphCanvas.ActualWidth / 2 + x_Offset)* zoomLevel;
            double halfHeight = (graphCanvas.ActualHeight / 2 + y_Offset)*zoomLevel;
            double val = 0;
            double increment = 0.5 * Math.Pow(2, zoomNum - 1);
            var result = (increment, new List<double> { 0, 0, 0, 0 });



            // Label the +ve X-axis
            for (double i = halfWidth ; i < graphCanvas.ActualWidth; i += 50 * zoomLevel)
            {
                TextBlock label = new TextBlock
                {
                    Text = (val).ToString(),
                    Foreground = Brushes.Black
                };

                Canvas.SetLeft(label, i);
                Canvas.SetTop(label, halfHeight + 5); // Adjust the vertical position as needed

                graphCanvas.Children.Add(label);
                val += increment;
                result.Item2[0] = val;
            }
            
            // Label the -ve X-axis
            val = 0;
            for (double i = halfWidth; i >= 0; i -= 50 * zoomLevel)
            {
                TextBlock label = new TextBlock
                {
                    Text = (val).ToString(),
                    Foreground = Brushes.Black
                };

                Canvas.SetLeft(label, i);
                Canvas.SetTop(label, halfHeight + 5); // Adjust the vertical position as needed

                graphCanvas.Children.Add(label);
                val -= increment;
                result.Item2[1] = val;
            }

            // Label the +ve Y-axis
            val = 0;
            for (double i = halfHeight; i > 0 ; i -= 50 * zoomLevel)
            {
                if (val != 0){
                    TextBlock label = new TextBlock
                    {
                        Text = (val).ToString(),
                        Foreground = Brushes.Black
                    };

                    Canvas.SetLeft(label, halfWidth + 5); // Adjust the horizontal position as needed
                    Canvas.SetTop(label, i);

                    graphCanvas.Children.Add(label);
                }
                val += increment;
                result.Item2[2] = val;
            }

            // Label the -ve Y-axis
            val = 0;
            for (double i = halfHeight; i < graphCanvas.ActualHeight; i += 50 * zoomLevel)
            {
                if (val != 0)
                {

                    TextBlock label = new TextBlock
                    {
                        Text = (val).ToString(),
                        Foreground = Brushes.Black
                    };

                    Canvas.SetLeft(label, halfWidth + 5); // Adjust the horizontal position as needed
                    Canvas.SetTop(label, i);

                    graphCanvas.Children.Add(label);
                }
                val -= increment;
                result.Item2[3] = val;
            }

            return result;
        }

        private void CheckZoomReset(double interval)
        {
            // Reset interval when it exceeds the maximum
            if (interval > baseInterval * 2)
            {
                zoomLevel = 1.0;  // Reset zoom level
                interval = baseInterval;  // Reset interval
                zoomNum -= 1;
            }
            else if (interval < baseInterval / 2)
            {
                zoomLevel = 1.0;  // Reset zoom level
                interval = baseInterval;  // Reset interval
                zoomNum += 1;
            }
        }

        private void DrawAxis()
        {
            Line xAxis = new Line
            {
                X1 = 0,
                Y1 = ((graphCanvas.ActualHeight / 2) + y_Offset)*zoomLevel,
                X2 = graphCanvas.ActualWidth,
                Y2 = ((graphCanvas.ActualHeight / 2) + y_Offset)*zoomLevel,
                Stroke = Brushes.Black,
                StrokeThickness = 2
            };
            graphCanvas.Children.Add(xAxis);

            Line yAxis = new Line
            {
                X1 = ((graphCanvas.ActualWidth / 2) + x_Offset) * zoomLevel,
                Y1 = 0,
                X2 = ((graphCanvas.ActualWidth / 2) + x_Offset) * zoomLevel,
                Y2 = graphCanvas.ActualHeight,
                Stroke = Brushes.Black,
                StrokeThickness = 2
            };
            graphCanvas.Children.Add(yAxis);
        }

        private List<Point> GeneratePoly(List<double> coefficients, double minX, double maxX, double step)
        {
            List<Point> points = new List<Point>();

            for (double x = minX; x <= maxX; x += step)
            {
                double y = EvaluatePolynomial(coefficients, x);

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
        
        private void DrawPoints(List<Point> points)
        {
            Polyline polyline = new Polyline
            {
                Stroke = Brushes.Blue,
                StrokeThickness = 2
            };

            foreach (Point point in points)
            {
                polyline.Points.Add(new Point(point.X , point.Y));
            }

            graphCanvas.Children.Add(polyline);
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
                DrawGraph(sender, e);

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
                zoomLevel *= 1.01; // Zoom in
            else
                zoomLevel /= 1.01; // Zoom out

            // Calculate the new pan offsets to keep the cursor at the same position after zooming
            //x_Offset = cursorPosition.X - (cursorPosition.X - x_Offset) * (zoomLevel);
            //y_Offset = cursorPosition.Y - (cursorPosition.Y - y_Offset) * (zoomLevel);

            // Redraw the graph with the new zoom level and pan offsets
            DrawGraph(sender, e);
        }
    }

}
