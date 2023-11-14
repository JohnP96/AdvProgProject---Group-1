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

            points = MapPointsToCanvas(points, scaleFactor)

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

                points.Add(new Point(x,y);
            }

            return points;
        }

        private List<Point> MapPointsToCanvas(List<Point> points, double canvasUnit)
        {
            foreach (var point in points)
            {
                double canvasX = MapXToCanvas(point.X, canvasUnit);
                double canvasY = MapYToCanvas(point.Y, canvasUnit);

                point.X = canvasX;
                point.Y = canvasY;

            }
            return points;
        }

        private double MapXToCanvas(double x, double ratio)
        {
            return x * ratio;
        }

        private double MapYToCanvas(double y, double ratio)
        {
            return y * ratio; 
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



        /*
        private void make_axis(int numSteps)
        {
            const double margin = 10;
            double xmin = -canGraph.Width / 2 + margin;
            double xmax = canGraph.Width / 2 - margin;
            double ymin = -canGraph.Height / 2 + margin;
            double ymax = canGraph.Height / 2 - margin;

            // Calculate the step size based on the canvas size and number of steps.
            double xStepSize = canGraph.Width / numSteps;
            double yStepSize = canGraph.Height / numSteps;

            // Make the X axis.
            GeometryGroup xaxis_geom = new GeometryGroup();
            xaxis_geom.Children.Add(new LineGeometry(
                new Point(0, canGraph.Height / 2), new Point(canGraph.Width, canGraph.Height / 2)));
            for (double x = 0; x <= canGraph.Width; x += xStepSize)
            {
                xaxis_geom.Children.Add(new LineGeometry(
                    new Point(x, canGraph.Height / 2 - margin / 2),
                    new Point(x, canGraph.Height / 2 + margin / 2)));

                // Add labels to the X axis.
                double xLabelValue = (x - canGraph.Width / 2) / xStepSize;
                if (!(xLabelValue >= -1 && xLabelValue <= 1))
                {
                    TextBlock label = new TextBlock();
                    label.Text = xLabelValue.ToString();
                    Canvas.SetLeft(label, x); // Adjust the label position.
                    Canvas.SetTop(label, canGraph.Height / 2 + margin);
                    canGraph.Children.Add(label);
                }
            }

            Path xaxis_path = new Path();
            xaxis_path.StrokeThickness = 1;
            xaxis_path.Stroke = Brushes.Black;
            xaxis_path.Data = xaxis_geom;

            canGraph.Children.Add(xaxis_path);

            // Make the Y axis.
            GeometryGroup yaxis_geom = new GeometryGroup();
            yaxis_geom.Children.Add(new LineGeometry(
                new Point(canGraph.Width / 2, 0), new Point(canGraph.Width / 2, canGraph.Height)));
            for (double y = 0; y <= canGraph.Height; y += yStepSize)
            {
                yaxis_geom.Children.Add(new LineGeometry(
                    new Point(canGraph.Width / 2 - margin / 2, y),
                    new Point(canGraph.Width / 2 + margin / 2, y)));

                // Add labels to the Y axis.
                double yLabelValue = -((y - canGraph.Height / 2) / yStepSize);
                if (!(yLabelValue >= -1 && yLabelValue <= 1))
                {
                    TextBlock label = new TextBlock();
                    label.Text = yLabelValue.ToString();
                    Canvas.SetLeft(label, canGraph.Width / 2 - 20);
                    Canvas.SetTop(label, y - 10); // Adjust the label position.
                    canGraph.Children.Add(label);
                }
            }

            Path yaxis_path = new Path();
            yaxis_path.StrokeThickness = 1;
            yaxis_path.Stroke = Brushes.Black;
            yaxis_path.Data = yaxis_geom;

            canGraph.Children.Add(yaxis_path);
        }

        public static PointCollection GeneratePolynomialPoints(double[] coefficients, int numSteps, double xmin, double xmax)
        {
            PointCollection points = new PointCollection();
            double xStep = (xmax - xmin) / numSteps;

            for (int i = 0; i <= numSteps; i++)
            {
                double x = xmin + i * xStep;
                double y = EvaluatePolynomial(coefficients, x);
                points.Add(new Point(x, y));
            }

            return points;
        }


        // Get the y value for a given x.
        public static double EvaluatePolynomial(double[] coefficients, double x)
        {
            double result = 0;
            for (int i = 0; i < coefficients.Length; i++)
            {
                result += coefficients[i] * Math.Pow(x, i);
            }
            return result;
        }

        private void draw_data(Canvas canvas, PointCollection points, Brush brush, double thickness)
        {
            Polyline polyline = new Polyline
            {
                Stroke = brush,
                StrokeThickness = thickness,
                Points = points
            };
            canvas.Children.Add(polyline);
        }
        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            int numSteps = 20; // Number of steps for both X and Y axes.

            // Draw X and Y axis with labeled steps.
            make_axis(numSteps);

            
            // Example usage:
            double[] coefficients = { 2, 3 }; // Represents 2x + 3
            double xmin = 0; // Adjust as needed
            double xmax = numSteps; // Adjust as needed

            // Generate points using the number of steps.
            PointCollection points = GeneratePolynomialPoints(coefficients, numSteps, xmin, xmax);

            draw_data(canGraph, points, Brushes.Blue, 2);

            // Draw data line

        }
        */
    }

}
