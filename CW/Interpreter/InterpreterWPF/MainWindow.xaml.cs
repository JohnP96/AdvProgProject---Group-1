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
                TextBlock label = new TextBlock();
                label.Text = ((x - canGraph.Width / 2) / xStepSize).ToString();
                Canvas.SetLeft(label, x - 10); // Adjust the label position.
                Canvas.SetTop(label, canGraph.Height / 2 + margin);
                canGraph.Children.Add(label);
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
                if (!(y >= -2 && y <= 2))
                {
                    TextBlock label = new TextBlock();
                    label.Text = (-((y - canGraph.Height / 2) / yStepSize)).ToString();
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

        public static PointCollection GeneratePolynomialPoints(double[] coefficients, double step, double xmin, double xmax)
        {
            PointCollection points = new PointCollection();
            for (double x = xmin; x <= xmax; x += step)
            {
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

            /*
            // Example usage:
            double[] coefficients = { 2, 3 }; // Represents 2x + 3
            double xmin = 0; // Adjust as needed
            double xmax = numSteps; // Adjust as needed

            // Generate points using the number of steps.
            PointCollection points = GeneratePolynomialPoints(coefficients, numSteps, xmin, xmax);

            draw_data(canGraph, points, Brushes.Blue, 2);

            // Draw data line

            */
        }



    }
}
