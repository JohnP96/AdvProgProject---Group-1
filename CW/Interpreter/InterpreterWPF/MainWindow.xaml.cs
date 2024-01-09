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
using pNeReturnVal = System.Tuple<bool, System.Tuple<Microsoft.FSharp.Collections.FSharpList<InterpreterFSharp.LexerParser.terminal>, System.Tuple<string, InterpreterFSharp.LexerParser.Number>>>;

namespace InterpreterWPF
{

    public partial class MainWindow : Window
    {
        // Graph Variables
        private double zoomLevel = 1;   // The zoom of the graph; resets when a limit is reached 
        private double x_Offset = 0;    // The offset of the graph in the x direcrtion 
        private double y_Offset = 0;    // The offset of the graph in the y direcrtion 

        private double baseInterval = 10; // Initial interval between grey grid lines
        private double baseDarkInterval = 50; // Initial interval between dark grid lines

        private double zoomNum = 2;     // Number of times the graph has been reset due to zooming;

        private Graph testGraph;

        FSharpList<stringValPair> symList;
        terminalList plotTokens;

        public MainWindow()
        {
            InitializeComponent();
            symList = FSharpList<stringValPair>.Empty;
            plotTokens = LexerParser.initPlotTokens;
            testGraph = new Graph(graphCanvas, zoomLevel, x_Offset, y_Offset, baseInterval, baseDarkInterval, zoomNum);

            cmdWindow.AppendText("Please enter an equation or type 'help' for more information:\n");
        }

        private void enterBtn_Click(object sender, RoutedEventArgs e)
        {
            bool success = true;
            if (Input.Text == "help" | Input.Text == "Help") 
            {
                cmdWindow.AppendText(
                    "\n> HELP:\n" +
                    "\n> You can enter an equation using the following operators:\n" +
                    "'+' and '-' for addition and subtraction\n" +
                    "'*' and '/' for multiplication and division\n" +
                    "'%' for calculating the remainder\n" +
                    "'^' for powers/indices\n" +
                    "then press the 'Enter' button or return key for the result.\n" +
                    "The result will also state whether it is an integer (Int) or float (Float) value.\n" +
                    "For example: '2^2*(2+2)' would give the result 'Int 16'\n" +
                    "\n> You can also assign strings as variables with the '=' operator\n" +
                    "For example: 'x = 12+1' would assign the value of 13 to the string 'x'.\n" +
                    "Assigned variables and their values can be seen in the box below.\n" +
                    "\n> You can use the keyword 'plot' to plot functions on the graph to the right.\n" +
                    "For example: 'plot x + 1' would plot that graph.\n" + 
                    "Remember not to use 'plot y = x + 1' as this will not work.\n" +
                    "\n> A list of tokens will also be returned representing the provided equation. " +
                    "The tokens represent different elements of the equation and can be help understand how the calculation is being done, " +
                    "most should be self explanatory, but typing 'helptokens' will return a list of these tokens and their meanings.\n\n"
                    );
            }
            else if (Input.Text == "helptokens")
            {
                cmdWindow.AppendText(
                    "\n> Tokens: \n\n" + 
                    "> Vid = Variable identifier (i.e. for 'x = 1' x is the Vid).\n" +
                    "> Neg = Unary minus (i.e. the minus in '-2 + 1').\n" +
                    "> Plus = Unary plus.\n" +
                    "> Num = A number, which can be an integer (Int) or float (Float) and will contain a specific value.\n" +
                    "> Plt = The plot keyword.\n" +
                    "> Equ = The assignment operator.\n" +
                    "> Lpar and Rpar = Left and right parenthesis." +
                    "> Pow = Power operator.\n" +
                    "> Rem = Modulus (remainder) operator.\n" +
                    "> Mul = Multiplication operator.\n" +
                    "> Div = Division operator.\n" +
                    "> Add = Addition operator.\n" +
                    "> Sub = Subtraction operator.\n"
                    );
            }
            else if (Input.Text != "")
            {
                cmdWindow.AppendText("> " + Input.Text + "\n");
                string input = Input.Text;
                terminalList lexed = LexerParser.lexer(input);
                for (int i = 0; i < lexed.Length; i++)
                {
                    if (lexed[i] is LexerParser.terminal.Err)
                    {
                        success = false;
                        cmdWindow.AppendText("> Error: " + lexed[i] + " is not a valid lexeme\n");
                    }
                }
                string parseRes = LexerParser.parser(lexed, symList).Item1.ToString();
                //cmdWindow.AppendText("> Parser result: " + parseRes + "\n // Testing
                if (parseRes.StartsWith("F"))
                {
                    success = false;
                    // Print error message
                    cmdWindow.AppendText(string.Concat("> ", parseRes.AsSpan(9, (parseRes.Length - 10)), "\n")); // The span gets rid of the success/failure notation and the quotation marks
                }
                else if (parseRes.Substring(9) != "]")
                {
                    success = false;
                    cmdWindow.AppendText("> Invalid expression.\n");
                }
                if (success)
                {
                    cmdWindow.AppendText("> Tokens: " + string.Join(", ", lexed) + "\n");
                    Tuple<pNeReturnVal, FSharpList<stringValPair>> result =
                        LexerParser.parseNevalNsym(lexed, symList);
                    LexerParser.Number answer = result.Item1.Item2.Item2.Item2;
                    symList = result.Item2;

                    if (result.Item1.Item1)
                    {
                        plotTokens = result.Item1.Item2.Item1;
                        DrawGraph2(sender, e);
                    }
                    else
                    {
                        cmdWindow.AppendText("> Result: " + answer + "\n");
                        //cmdWindow.AppendText("> Sym: " + symList + "\n"); // Testing
                    }
                    
                }
                string vars = "\n";
                foreach (stringValPair var in symList)
                {
                    vars = vars + var.Item1 + " = " + var.Item2 + "\n";
                }
                VariableTracker.Text = "Variables:" + vars;
            }
              
            cmdWindow.ScrollToEnd();
            Input.Clear();
        }



        private void DrawGraph2(object sender, RoutedEventArgs e)
        {
            // Clear everything on the canvas
            testGraph.clear(graphCanvas);

            double darkInterval = baseDarkInterval * zoomLevel;
            double interval = baseInterval * zoomLevel;

            // Draw Axis
            testGraph.drawAxis(graphCanvas, x_Offset, y_Offset, zoomLevel);

            // Draw grid lines
            List<double> remainder = testGraph.drawGridLines(graphCanvas, ref interval, baseInterval,darkInterval, x_Offset, y_Offset, ref zoomLevel, ref zoomNum);
            
            // Draw Labels
            (double increment, List<double> minLabels) = testGraph.drawLabels(graphCanvas, x_Offset,y_Offset,zoomLevel, zoomNum); // Draws the labels and returns the value of each black line and the last label

            // calculate minX and maxX of Graph 
            List<double> resi = CalculateMinAndMax(remainder, increment, minLabels);

            double step = 0.1;
            double scaleFactor = Math.Abs(baseInterval * zoomLevel);

            // Generate Polynomial data
            //List<double> coefficients = new List<double> { 1, 1}; // Represents x^2 + x

            List<Point> points = GeneratePoly(resi[1], resi[0], step);


            points = MapPointsToCanvas(points, scaleFactor);

            testGraph.DrawPoints(graphCanvas, points);
        }

        //private void DrawGraph(object sender, RoutedEventArgs e)
        //{
        //    // Clear the Grid lines 
        //    graphCanvas.Children.Clear();

        //    // Draw grid lines
        //    double darkInterval = baseDarkInterval * zoomLevel;
        //    double interval = baseInterval * zoomLevel;
        //    List<double> remainder = DrawGridLines(interval, darkInterval); //  Draws the grid and Calculates how many grey lines after the last black line

        //    // Draw axes
        //    DrawAxis();

        //    // Draw Labels
        //    (double increment, List<double> minLabels) = DrawLabels(); // Draws the labels and returns the value of each black line and the last label

        //    // Generate Polynomial data
        //    //List<double> coefficients = new List<double> { 1, 1}; // Represents x^2 + x


        //    // calculate minX and maxX
        //    List<double> resi = CalculateMinAndMax(remainder, increment, minLabels);
        //    //double minX = resi[1];
        //    //int maxX = resi[0]+1;
        //    double step = 0.1;
        //    double scaleFactor = Math.Abs(baseInterval * zoomLevel);

        //    List<Point> points = GeneratePoly(resi[1], resi[0], step);


        //    points = MapPointsToCanvas(points, scaleFactor);

        //    DrawPoints(points);
        //}

        
        private List<double> CalculateMinAndMax(List<double> remainder, double increment, List<double> minLabels)
        {
            for (int i = 0; i < minLabels.Count; i++)
            {
                minLabels[i] += (increment/(baseDarkInterval/baseInterval)) * remainder[i];
            }

            return minLabels;
        }

        private List<Point> GeneratePoly(double minX, double maxX, double step)
        {
            List<Point> points = new List<Point>();

            for (double x = minX; x <= maxX; x += step)
            {
                String res = LexerParser.evalPoly(plotTokens, x).ToString();
                res = res.Substring(6);
                //cmdWindow.AppendText(res.ToString());// Testing

                double y = Convert.ToDouble(res);
                //double y = x + 1;

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

                if (plotTokens != null)
                {
                    // Redraw the graph with the new pan offset
                    DrawGraph2(sender, e);
                }

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
                zoomLevel *= 1.03; // Zoom in
            else
                zoomLevel /= 1.03; // Zoom out

            // Calculate the new pan offsets to keep the cursor at the same position after zooming
            //x_Offset = cursorPosition.X - (cursorPosition.X - x_Offset) * (zoomLevel);
            //y_Offset = cursorPosition.Y - (cursorPosition.Y - y_Offset) * (zoomLevel);

            if (plotTokens != null)
            {
                // Redraw the graph with the new zoom level and pan offsets
                DrawGraph2(sender, e);
            }
        }
    }

}
