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


namespace InterpreterWPF
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        FSharpList<Tuple<string, LexerParser.Number>> symList;

        public MainWindow()
        {
            InitializeComponent();
            symList = FSharpList<Tuple<string, LexerParser.Number>>.Empty;
        }

        private void enterBtn_Click(object sender, RoutedEventArgs e)
        {
            bool flag = true;
            cmdWindow.AppendText("> " + Input.Text + "\n");
            string input = Input.Text;
            FSharpList<LexerParser.terminal> lexed = LexerParser.lexer(input);
            for (int i = 0; i < lexed.Length; i++){
                if (lexed[i] is LexerParser.terminal.Err)
                {
                    flag = false;
                    cmdWindow.AppendText("> Error: " + lexed[i] + " is not a valid lexeme");
                }
            }
            string  parseRes = LexerParser.parser(lexed).ToString();
            
            if (parseRes.StartsWith("F"))
            {
                flag = false;
                cmdWindow.AppendText(parseRes);
            }
            if (flag)
            {
                cmdWindow.AppendText("> Tokens: " + string.Join(", ", lexed) + "\n");
                Tuple<string, LexerParser.Number> result =
                    LexerParser.parseNeval(lexed, symList).Item2;
                LexerParser.Number answer = result.Item2;
                if (result.Item1 != "")
                {
                    symList = FSharpList<Tuple<string, LexerParser.Number>>.Cons(
                        Tuple.Create(result.Item1, answer), symList);
                }
                cmdWindow.AppendText("> Result: " + answer + "\n");
                cmdWindow.ScrollToEnd();
            }
            Input.Clear();
        }

         // Graph Variables
        private double zoomLevel = 1;
        private double x_Offset = 0;
        private double y_Offset = 0;

        private double baseInterval = 10; // Initial interval between grey grid lines
        private double baseDarkInterval = 50; // Initial interval between dark grid lines

        private double zoomNum = 1;


        private void DrawGraph(object sender, RoutedEventArgs e)
        {
            // Clear the Grid lines 
            graphCanvas.Children.Clear();
            // Draw grid lines
            double darkInterval = baseDarkInterval * zoomLevel;
            double interval = baseInterval * zoomLevel;
            DrawGridLines(interval, darkInterval);

            // Draw axes
            DrawAxis();

            // Draw Labels
            DrawLabels();

            // Generate Polynomial data
            List<double> coefficients = new List<double> {1, 0}; // Represents x^2 - 3x + 2
            //double minX = -graphCanvas.Width/2;
            //double maxX = graphCanvas.Width/2;
            double minX = -50;
            double maxX = 50;
            double step = 1;
            double scaleFactor = Math.Abs(baseInterval * zoomLevel);

            List<Point> points  = GeneratePoly(coefficients, minX, maxX, step);

            points = MapPointsToCanvas(points, scaleFactor);

            DrawPoints(points);
        }
        
        private void DrawLabels()
        {
            double halfWidth = (graphCanvas.ActualWidth / 2 + x_Offset)* zoomLevel;
            double halfHeight = (graphCanvas.ActualHeight / 2 + y_Offset)*zoomLevel;
            double val = 0;
            double increment = 0.5 * Math.Pow(2, zoomNum - 1);



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

            }
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

        private void DrawGridLines(double interval, double darkInterval)
        {
            double halfWidth = ((graphCanvas.ActualWidth / 2) + x_Offset)*zoomLevel;
            double halfHeight = (( graphCanvas.ActualHeight / 2) + y_Offset)*zoomLevel;

            // Draw light gray grid lines


            // Check if the Grid needs to be reset
            CheckZoomReset(interval);

            // Draw light gray grid lines
            for (double x = halfWidth; x <= graphCanvas.ActualWidth; x += interval)
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
