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


public class Graph
{
    // Graph Variables
    private double zoomLevel = 1;
    private double x_Offset = 0;
    private double y_Offset = 0;
    private double baseInterval = 10;
    private double baseDarkInterval = 50;
    private double zoomNum = 2;

    // Canvas
    private Canvas graphCanvas;

    // Constructor
    public Graph(Canvas canvas, double zoomLevel, double x_Offset, double y_Offset, double baseInterval, double baseDarkInterval, double zoomNum)
    {
        // Set values for the variables 
        this.zoomLevel = zoomLevel;
        this.x_Offset = x_Offset;
        this.y_Offset = y_Offset;
        this.baseInterval = baseInterval;
        this.baseDarkInterval = baseDarkInterval;
        this.zoomNum = zoomNum;

        // Canvas
        this.graphCanvas = canvas;

    }

    // Fucntions
    private void DrawLine(Canvas canvas, double x1, double y1, double x2, double y2, Brush strokeColor, double thickness)
    {
        Line line = new Line
        {
            X1 = x1,
            Y1 = y1,
            X2 = x2,
            Y2 = y2,
            Stroke = strokeColor,
            StrokeThickness = thickness
        };
        canvas.Children.Add(line);
    }
    private void CheckZoomReset(ref double interval, double baseInterval, ref double zoomLevel, ref double zoomNum)
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
   
    // METHODS
    public void clear(Canvas graphCanvas)
    {
        graphCanvas.Children.Clear();
    }

    // Draw Axis
    public void drawAxis(Canvas graphCanvas, double x_Offset, double y_Offset, double zoomLevel)
    {
        // X-Axis
        DrawLine(graphCanvas, 0, ((graphCanvas.ActualHeight / 2) + y_Offset) * zoomLevel, graphCanvas.ActualWidth, ((graphCanvas.ActualHeight / 2) + y_Offset) * zoomLevel, Brushes.Black, 2);

        // Y-Axis
        DrawLine(graphCanvas, ((graphCanvas.ActualWidth / 2) + x_Offset) * zoomLevel, 0, ((graphCanvas.ActualWidth / 2) + x_Offset) * zoomLevel, graphCanvas.ActualHeight, Brushes.Black, 2);
    }

    // Draw Grid lines
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
    public List<double> drawGridLines(Canvas graphCanvas, ref double interval, double baseInterval, double darkInterval, double x_Offset, double y_Offset, ref double zoomLevel, ref double zoomNum)
    {
        List<double> greyLines = new List<double> { 0, 0, 0, 0 }; // Keep track of how many grey lines after a black line
        List<double> blackLines = new List<double> { 0, 0, 0, 0 }; // Keep track of the number of black lines

        double halfWidth = ((graphCanvas.ActualWidth / 2) + x_Offset) * zoomLevel;
        double halfHeight = ((graphCanvas.ActualHeight / 2) + y_Offset) * zoomLevel;

        // Check if the Grid needs to be reset
        CheckZoomReset(ref interval, baseInterval, ref zoomLevel, ref zoomNum);

        // Draw light gray grid lines
        for (double x = halfWidth; x <= graphCanvas.ActualWidth; x += interval) // +ve X-axis
        {
            DrawLine(graphCanvas, x, 0, x, graphCanvas.ActualHeight, Brushes.LightGray, 1);
            greyLines[0]++;
        }

        for (double x = halfWidth; x >= 0; x -= interval)
        {
            DrawLine(graphCanvas, x, 0, x, graphCanvas.ActualHeight, Brushes.LightGray, 1);
            greyLines[1]++;
        }

        for (double y = halfHeight; y <= graphCanvas.ActualHeight; y += interval)
        {
            DrawLine(graphCanvas, 0, y, graphCanvas.ActualWidth, y, Brushes.LightGray, 1);
            greyLines[2]++;
        }

        for (double y = halfHeight; y >= 0; y -= interval)
        {
            DrawLine(graphCanvas, 0, y, graphCanvas.ActualWidth, y, Brushes.LightGray, 1);
            greyLines[3]++;
        }

        // Draw dark gray grid lines with a larger interval
        for (double x = halfWidth; x <= graphCanvas.ActualWidth; x += darkInterval)
        {
            DrawLine(graphCanvas, x, 0, x, graphCanvas.ActualHeight, Brushes.Black, 1);
            blackLines[0]++;
        }

        for (double x = halfWidth; x >= 0; x -= darkInterval)
        {
            DrawLine(graphCanvas, x, 0, x, graphCanvas.ActualHeight, Brushes.Black, 1);
            blackLines[1]++;
        }

        for (double y = halfHeight; y <= graphCanvas.ActualHeight; y += darkInterval)
        {
            DrawLine(graphCanvas, 0, y, graphCanvas.ActualWidth, y, Brushes.Black, 1);
            blackLines[2]++;
        }

        for (double y = halfHeight; y >= 0; y -= darkInterval)
        {
            DrawLine(graphCanvas, 0, y, graphCanvas.ActualWidth, y, Brushes.Black, 1);
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

    // Draw Labels
    /*
    Func:-      DrawLabels()

    Return:-    result(Tuple):  [0] - The value of each black line
                                [1] - List of The last label drawn in each sector [+x,-x,+y,-y]
    */
    public (double, List<double>) drawLabels(Canvas graphCanvas, double x_Offset, double y_Offset, double zoomLevel, double zoomNum)
    {
        double halfWidth = (graphCanvas.ActualWidth / 2 + x_Offset) * zoomLevel;
        double halfHeight = (graphCanvas.ActualHeight / 2 + y_Offset) * zoomLevel;
        double val = 0;
        double increment = 0.5 * Math.Pow(2, zoomNum - 1);
        var result = (increment, new List<double> { 0, 0, 0, 0 });



        // Label the +ve X-axis
        for (double i = halfWidth; i < graphCanvas.ActualWidth; i += 50 * zoomLevel)
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
        for (double i = halfHeight; i > 0; i -= 50 * zoomLevel)
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

    // Draw Poly
    public void DrawPoints(Canvas graphCanvas, List<Point> points)
    {
        Polyline polyline = new Polyline
        {
            Stroke = Brushes.Blue,
            StrokeThickness = 2
        };

        foreach (Point point in points)
        {
            polyline.Points.Add(new Point(point.X, point.Y));
        }

        graphCanvas.Children.Add(polyline);
    }

    public void DrawDot(Canvas graphCanvas, List<Point> points)
    {
        foreach (Point point in points)
        {
            Ellipse dot = new Ellipse
            {
                Width = 10,
                Height = 10,
                Fill = Brushes.ForestGreen
            };

            Canvas.SetLeft(dot, point.X - 5);
            Canvas.SetTop(dot, point.Y - 5);
            Canvas.SetZIndex(dot, 100);

            graphCanvas.Children.Add(dot);
        }
    }
}

