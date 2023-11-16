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

         // Graph 
        private double zoomLevel = 1.0;
        private double x_Offset = 0;
        private double y_Offset = 0;

        private void DrawGraph(object sender, RoutedEventArgs e)
        {
            // Clear the Grid lines 
            graphCanvas.Children.Clear();
            // Draw grid lines, axes and Indents
            DrawGridLines();
            DrawAxis(x_Offset, y_Offset);
            DrawLabels(x_Offset, y_Offset);

            // Generate Polynomial data
            List<double> coefficients = new List<double> {1,0,0 }; // Represents x^2 - 3x + 2
            //double minX = -graphCanvas.Width/2;
            //double maxX = graphCanvas.Width/2;
            double minX = -20;
            double maxX = 20;
            double step = 0.5;
            double scaleFactor = 10 * zoomLevel;

            List<Point> points  = GeneratePoly(coefficients, minX, maxX, step);

            points = MapPointsToCanvas(points, scaleFactor);

            DrawPoints(points);
        }
        
        private void DrawLabels(double xOffset, double yOffest)
        {
            double halfWidth = graphCanvas.ActualWidth / 2 + xOffset;
            double halfHeight = graphCanvas.ActualHeight / 2 + yOffest;
            double val = 0;

            // Label the +ve X-axis
            for (double i = halfWidth ; i < graphCanvas.ActualWidth; i += 50)
            {
                TextBlock label = new TextBlock
                {
                    Text = (val).ToString(),
                    Foreground = Brushes.Black
                };

                Canvas.SetLeft(label, i);
                Canvas.SetTop(label, halfHeight + 5); // Adjust the vertical position as needed

                graphCanvas.Children.Add(label);
                val++;
            }
            
            // Label the -ve X-axis
            val = 0;
            for (double i = halfWidth; i >= 0; i -= 50)
            {
                TextBlock label = new TextBlock
                {
                    Text = (val).ToString(),
                    Foreground = Brushes.Black
                };

                Canvas.SetLeft(label, i);
                Canvas.SetTop(label, halfHeight + 5); // Adjust the vertical position as needed

                graphCanvas.Children.Add(label);
                val--;
            }
            
            // Label the +ve Y-axis
            val = 1;
            for (double i = halfHeight - 50; i > 0 ; i -= 50)
            {
                TextBlock label = new TextBlock
                {
                    Text = (val).ToString(),
                    Foreground = Brushes.Black
                };

                Canvas.SetLeft(label, halfWidth + 5); // Adjust the horizontal position as needed
                Canvas.SetTop(label, i);

                graphCanvas.Children.Add(label);
                val++;
            }

            // Label the -ve Y-axis
            val = -1;
            for (double i = halfHeight + 50; i < graphCanvas.ActualHeight; i += 50)
            {
                TextBlock label = new TextBlock
                {
                    Text = (val).ToString(),
                    Foreground = Brushes.Black
                };

                Canvas.SetLeft(label, halfWidth + 5); // Adjust the horizontal position as needed
                Canvas.SetTop(label, i);

                graphCanvas.Children.Add(label);
                val--;
            }
        }

        private void DrawGridLines()
        {
            double halfWidth = (graphCanvas.ActualWidth / 2) + x_Offset;
            double halfHeight = ( graphCanvas.ActualHeight / 2) + y_Offset;

            // Draw light gray grid lines

            // Define a scaling factor for the grid lines
            double gridScaleFactor = 1.0;
            double cumulativeScaledDistance = 0.0;

            double baseInterval = 10; // Initial interval between grid lines
            double maxInterval = 50;  // Maximum interval between grid lines
            double interval = baseInterval * zoomLevel;

            // Reset interval when it exceeds the maximum
            if (interval > maxInterval)
            {
                zoomLevel = 1.0;  // Reset zoom level
                interval = baseInterval;  // Reset interval
            }

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
            for (double x = halfWidth; x <= graphCanvas.ActualWidth; x += maxInterval)
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

            for (double x = halfWidth - 50; x >= 0; x -= 50)
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

            for (double y = halfHeight; y <= graphCanvas.ActualHeight; y += 50)
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

            for (double y = halfHeight - 50; y >= 0; y -= 50)
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

        private void DrawAxis(double xOffset, double yOffset)
        {
            Line xAxis = new Line
            {
                X1 = 0,
                Y1 = (graphCanvas.ActualHeight / 2) + yOffset,
                X2 = graphCanvas.ActualWidth,
                Y2 = (graphCanvas.ActualHeight / 2) + yOffset,
                Stroke = Brushes.Black,
                StrokeThickness = 2
            };
            graphCanvas.Children.Add(xAxis);

            Line yAxis = new Line
            {
                X1 = (graphCanvas.ActualWidth / 2) + xOffset,
                Y1 = 0,
                X2 = (graphCanvas.ActualWidth / 2) + xOffset,
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
            return x * ratio;
        }

        private double MapYToCanvas(double y, double ratio)
        {
            return y * ratio; 
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
                polyline.Points.Add(new Point(point.X + graphCanvas.Width / 2, graphCanvas.Height / 2 - point.Y));
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
