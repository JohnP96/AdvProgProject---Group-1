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


public class GraphOperations
{

    // Variables 
    private Point panStartPoint;
    private Point panLastPoint;

    // Events
    public event Action<object, RoutedEventArgs> RedrawGraph;


    // Canvas to perform zooming and panning operations on 
    private Canvas graphCanvas;
    private Graph testGraph;

    // Constructor to initialize the canvas
    public GraphOperations(Canvas canvas, Graph graph)
    {
        graphCanvas = canvas;
        testGraph = graph;
        InitializeEventHandlers();
    }

    private void InitializeEventHandlers()
    {
        graphCanvas.MouseWheel += zoom;
        graphCanvas.MouseDown += mouseDown;
        graphCanvas.MouseMove += graphCanvas_MouseMove;
    }



    // Panning
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
            testGraph.x_Offset += deltaX;
            testGraph.y_Offset += deltaY;

            // Trigger event to redraw the graph
            RedrawGraph?.Invoke(sender, e);

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
            testGraph.zoomLevel *= 1.04; // Zoom in
        else
            testGraph.zoomLevel /= 1.04; // Zoom out

        // Redraw the graph with the new zoom level and pan offsets
        RedrawGraph?.Invoke(sender, e);
    }
}
