using System;
using System.Collections.Generic;
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
using System.Diagnostics;


namespace InterpreterWPF
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void enterBtn_Click(object sender, RoutedEventArgs e)
        {
            
            cmdWindow.AppendText("> " + Input.Text + "\n");
            string input = Input.Text;
            Microsoft.FSharp.Collections.FSharpList<LexerParser.terminal> lexed = LexerParser.lexer(input);
            cmdWindow.AppendText("> Tokens: " + string.Join(", ",lexed) + "\n");
            int answer = LexerParser.parseNeval(lexed).Item2;
            cmdWindow.AppendText("> Result: " + answer + "\n");
            Input.Clear();
        }

        // Graph 
        private double zoomLevel = 1.0;

        private void DrawGraph(object sender, RoutedEventArgs e)
        {
            // Draw grid lines, axes and Indents
            DrawGridLines();
            DrawAxis();
            DrawIndents();

            // Generate Polynomial data
            List<double> coefficients = new List<double> {1,0,0 }; // Represents x^2 - 3x + 2
            //double minX = -graphCanvas.Width/2;
            //double maxX = graphCanvas.Width/2;
            double minX = -20;
            double maxX = 20;
            double step = 0.5;
            double scaleFactor = 10;

            List<Point> points  = GeneratePoly(coefficients, minX, maxX, step);

            points = MapPointsToCanvas(points, scaleFactor);

            DrawPoints(points);
        }

        private void DrawGridLines()
        {
            for (double x = 0; x <= graphCanvas.Width; x += 10)
            {
                Line line = new Line
                {
                    X1 = x,
                    Y1 = 0,
                    X2 = x,
                    Y2 = graphCanvas.Height,
                    Stroke = Brushes.LightGray
                };
                graphCanvas.Children.Add(line);
            }

            for (double y = 0; y <= graphCanvas.Height; y += 10)
            {
                Line line = new Line
                {
                    X1 = 0,
                    Y1 = y,
                    X2 = graphCanvas.Width,
                    Y2 = y,
                    Stroke = Brushes.LightGray
                };
                graphCanvas.Children.Add(line);
            }
        }

        private void DrawAxis()
        {
            Line xAxis = new Line
            {
                X1 = 0,
                Y1 = graphCanvas.Height / 2,
                X2 = graphCanvas.Width,
                Y2 = graphCanvas.Height / 2,
                Stroke = Brushes.Black,
                StrokeThickness = 2
            };
            graphCanvas.Children.Add(xAxis);

            Line yAxis = new Line
            {
                X1 = graphCanvas.Width / 2,
                Y1 = 0,
                X2 = graphCanvas.Width / 2,
                Y2 = graphCanvas.Height,
                Stroke = Brushes.Black,
                StrokeThickness = 2
            };
            graphCanvas.Children.Add(yAxis);
        }

        private void DrawIndents()
        {
            // X-Axis
            for (double i = 0; i <= graphCanvas.Width; i += 10)
            {
                Line xAxis = new Line
                {
                    X1 = i,
                    Y1 = (graphCanvas.Height / 2) - 5,
                    X2 = i,
                    Y2 = (graphCanvas.Height / 2) + 5,
                    Stroke = Brushes.Black,
                    StrokeThickness = 2
                };
                graphCanvas.Children.Add(xAxis);
            }
            // Y-Axis
            for (double i = 0; i <= graphCanvas.Height; i += 10)
            {
                Line yAxis = new Line
                {
                    X1 = (graphCanvas.Width / 2) - 5,
                    Y1 = i,
                    X2 = (graphCanvas.Width / 2) + 5,
                    Y2 = i,
                    Stroke = Brushes.Black,
                    StrokeThickness = 2
                };
                graphCanvas.Children.Add(yAxis);
            }
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

        private void graphCanvas_MouseWheel(object sender, MouseWheelEventArgs e)
        {
            // Adjust zoom level based on mouse wheel delta
            if (e.Delta > 0)
                zoomLevel *= 1.1; // Zoom in
            else
                zoomLevel /= 1.1; // Zoom out

            // Apply the zoom level to the canvas transform
            canvasTransform.Matrix = new Matrix(zoomLevel, 0, 0, -zoomLevel, 0, 0);
        }
        
    }

}
