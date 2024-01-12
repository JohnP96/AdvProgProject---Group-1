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

public class MathOperations
{

    // Canvas to perform zooming and panning operations on 
    private Graph testGraph;
    private Canvas graphCanvas;


    // Constructor to initialize the canvas
    public MathOperations(Canvas canvas, Graph graph)
    {
        graphCanvas = canvas;
        testGraph = graph;
    }

    public void PlotGraph(terminalList plotTokens, List<double> resi, double start_, double stop_, double step, double scaleFactor)
    {
        List<Point> points = new List<Point>();

        // Plot Graph
        if (start_ == 0 && stop_ == 0)
        {
            points = GeneratePoints(resi[1], resi[0], step, plotTokens);
        }
        else
        {
            points = GeneratePoints(start_, stop_, step, plotTokens);
        }

        points = MapPointsToCanvas(points, scaleFactor);
        testGraph.DrawPoints(points, "Blue");
    }

    public void PlotDerivative(terminalList derivative, List<double> resi, double step, double scaleFactor)
    {
        // Plot Derivative
        List<Point> points = GeneratePoints(resi[1], resi[0], step, derivative);
        points = MapPointsToCanvas(points, scaleFactor);
        testGraph.DrawPoints(points, "Red");
    }

    public string PlotIntegral(terminalList integral, List<double> resi, double start_, double stop_, double step, double scaleFactor)
    {
        // Points for the plot
        List<Point> points = new List<Point>();

        // Area
        string area;

        // If limits not specified use the max and min X values on the graph
        if (start_ == 0 && stop_ == 0)
        {
            // Generate Points
            points = GeneratePoints(resi[1], resi[0], step, integral);

            // No limits specified area = NaN
            area = "NaN";
        }
        // Limits specified
        else
        {
            // Generate points
            points = GeneratePoints(start_,stop_, step, integral);

            // Calulate the Area btw start_ and stop_
            area = calcArea(integral, start_, stop_);
        }

        points = MapPointsToCanvas(points, scaleFactor);
        testGraph.DrawPoints(points, "Purple");

        return area;
    }

    public void MarkRoots(FSharpList<double> roots, double scaleFactor)
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

    public string calcArea(terminalList func, double start, double stop)
    {

        // Remove the string "Float" or "Int" using regular expression and convert to double; more detailed in func GenPoints
        double a1 = Convert.ToDouble(Regex.Replace(LexerParser.evalPoly(func, start).ToString(), @"\b(Float|Int)\b", ""));
        double a2 = Convert.ToDouble(Regex.Replace(LexerParser.evalPoly(func, stop).ToString(), @"\b(Float|Int)\b", ""));

        return Math.Abs(a2 - a1).ToString();
    }

    public void ShadeAreaUnderGraph(terminalList func, List<double> resi, double start_, double stop_, double scaleFactor)
    {
        List<Point> points = new List<Point>();
        if (start_ == 0 && stop_ == 0)
        {
            points = GeneratePoints(resi[1], resi[0], 0.1, func);
        }
        else
        {
            points = GeneratePoints(start_, stop_, 0.1, func);
        }
        points = MapPointsToCanvas(points, scaleFactor);

        PathFigure pathFigure = new PathFigure();
        pathFigure.StartPoint = new Point(points.First().X, ((graphCanvas.ActualHeight / 2) + testGraph.y_Offset) * testGraph.zoomLevel);

        foreach (var point in points)
        {
            pathFigure.Segments.Add(new LineSegment(point, true));
        }

        // Close the path with a line to the x-axis
        pathFigure.Segments.Add(new LineSegment(new Point(points.Last().X, ((graphCanvas.ActualHeight / 2) + testGraph.y_Offset) * testGraph.zoomLevel), true));

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

    public List<double> CalculateMinAndMax(List<double> remainder, double increment, List<double> minLabels)
    {
        List<Point> p = new List<Point>();
        for (int i = 0; i < minLabels.Count; i++)
        {
            minLabels[i] += (increment / (testGraph.baseDarkInterval / testGraph.baseInterval)) * remainder[i];
        }

        return minLabels;
    }

    public List<Point> GeneratePoints(double minX, double maxX, double step, terminalList func)
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

    public double EvaluatePolynomial(List<double> coefficients, double x)
    {
        double result = 0;
        for (int i = 0; i < coefficients.Count; i++)
        {
            result += coefficients[i] * Math.Pow(x, coefficients.Count - 1 - i);
        }
        return result;
    }

    public List<Point> MapPointsToCanvas(List<Point> points, double canvasUnit)
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


    public double MapXToCanvas(double x, double ratio)
    {
        double d = (testGraph.baseInterval * x) / (0.5 * Math.Pow(2, testGraph.zoomNum - 1) / 5);
        d += graphCanvas.ActualWidth / 2;
        d += testGraph.x_Offset;

        return d * testGraph.zoomLevel;
    }

    public double MapYToCanvas(double y, double ratio)
    {
        double d = (testGraph.baseInterval * y) / (0.5 * Math.Pow(2, testGraph.zoomNum - 1) / 5);
        double res = graphCanvas.ActualHeight / 2 - d;
        res += testGraph.y_Offset;
        return res * testGraph.zoomLevel;
    }
}
