//using System;
//using System.Windows;
using System.Windows.Controls;

using System.Windows.Media;
using System.Windows.Shapes;


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
    private void clear()
    {
        graphCanvas.Children.Clear();
    }

    // Draw Axis
    private void DrawAxis(Canvas graphCanvas, double x_Offset, double y_Offset, double zoomLevel)
    {
        // X-Axis
        DrawLine(graphCanvas, 0, ((graphCanvas.ActualHeight / 2) + y_Offset) * zoomLevel, graphCanvas.ActualWidth, ((graphCanvas.ActualHeight / 2) + y_Offset) * zoomLevel, Brushes.Black, 2);
        
        // Y-Axis
        DrawLine(graphCanvas, ((graphCanvas.ActualWidth / 2) + x_Offset) * zoomLevel, 0, ((graphCanvas.ActualWidth / 2) + x_Offset) * zoomLevel, graphCanvas.ActualHeight, Brushes.Black, 2);
    }

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

}