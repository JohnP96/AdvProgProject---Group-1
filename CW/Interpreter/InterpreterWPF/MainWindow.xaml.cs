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

namespace InterpreterWPF
{

    public partial class MainWindow : Window
    {
        private Graph testGraph;
        private GraphOperations graphOperator;
        private MathOperations mathOperator;



        FSharpList<stringValPair> symList;
        terminalList plotTokens;
        terminalList derivative;
        terminalList integral;
        LexerParser.Number start_;
        LexerParser.Number stop_;

        bool plot_ = false;
        bool integration_ = false;

        public MainWindow()
        {
            InitializeComponent();
            symList = FSharpList<stringValPair>.Empty;
            testGraph = new Graph(graphCanvas, 1, 0, 0, 10, 50, 2);
            graphOperator = new GraphOperations(graphCanvas, testGraph);
            mathOperator = new MathOperations(graphCanvas, testGraph);

            // Subscribe to the RedrawGraph event
            graphOperator.RedrawGraph += DrawGraph2;

        }

        private void enterBtn_Click(object sender, RoutedEventArgs e)
        {
            // Print to the screen the thing the user typed
            cmdWindow.AppendText("> " + Input.Text + "\n");

            // Remove the whitespaces from the input and make it lowercased  
            string input = Input.Text.Replace(" ", string.Empty).ToLower();

            // Call the lexer on the input and return a tokenList of the input
            terminalList lexed = LexerParser.lexer(input);

            // Add the token "Mul! btw the token "Num" and "Vid" i.e 2x -> 2*x
            lexed = LexerParser.insertMulBetweenNumAndVid(lexed);

            // value to check Parsing is sucessfull
            bool success;

            // look through the lexed list for the token "Err" and throw err if any is found 
            for (int i = 0; i < lexed.Length; i++)
            {
                if (lexed[i] is LexerParser.terminal.Err)
                {
                    success = false;
                    cmdWindow.AppendText("> Error: " + lexed[i] + " is not a valid lexeme\n");
                }
            }

            // Call the parser on the Lexed input retures Sucess[] if all good
            string parseRes = LexerParser.parser(lexed, symList).Item1.ToString();

            // Check if the parsing Failed
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
            else
            {
                success = true;
            }

            // If Parsing was successful
            if (success)
            {
                // Show the lexed tokens on the screen
                //cmdWindow.AppendText("> Tokens: " + string.Join(", ", lexed) + "\n");

                // Result => ((plot, (Integration, (start, stop)), ([tList], (vName, value))), [symList]).... i.e -> ((True,([Lpar; Num (Int 2); Mul; ... ], ("", Int 0)), [])
                var result = LexerParser.parseNevalNsym(lexed, symList);

                // Value representing if the req was to plot
                plot_ = result.Item1.Item1; 

                // Value representing if the req was to integrate
                integration_ = result.Item1.Item2.Item1;

                // Value representing the start and stop value for the plot or integration, default 0,0
                start_ = result.Item1.Item2.Item2.Item1;
                stop_ = result.Item1.Item2.Item2.Item2;

                // "answer" -> value
                LexerParser.Number answer = result.Item1.Item3.Item2.Item2;

                // "symList" -> SymList
                symList = result.Item2;

                // request was to plot
                if (plot_)
                {
                    // f(x)
                    plotTokens = result.Item1.Item3.Item1;

                    // f'(x)
                    derivative = LexerParser.findDerivative(plotTokens);
                    
                    // Print the derivative string to the card 
                    Info_derivative.Text = "Derivative: " + LexerParser.tokenToString(LexerParser.simplifyTokens(derivative)) + "\n";

                    // Draw the graph
                    DrawGraph2(sender, e);
                }
                //  Request was to integrate
                else if (integration_)
                {
                    // f(x)
                    plotTokens = result.Item1.Item3.Item1;

                    // ~f(x)
                    integral = LexerParser.findIntegral(plotTokens);

                    // Print the Integral string to the card 
                    Info_derivative.Text = "Integral: " + LexerParser.tokenToString(LexerParser.simplifyTokens(integral)) + "\n";

                    DrawGraph2(sender, e);

                }
                // Request was to solve an expression
                else
                {
                    cmdWindow.AppendText("> Result: " + answer + "\n");
                    //cmdWindow.AppendText("> Sym: " + symList + "\n"); // Testing
                }
                cmdWindow.ScrollToEnd();
            }
            string vars = "\n";
            foreach (stringValPair var in symList)
            {
                vars = vars + var.Item1 + " = " + var.Item2 + "\n";
            }
            VariableTracker.Text = "Variables:" + vars;
            Input.Clear();
        }



        private void DrawGraph2(object sender, RoutedEventArgs e)
        {
            // Clear everything on the canvas
            testGraph.clear();

            // Draw Axis
            testGraph.drawAxis();

            // Draw grid lines
            List<double> remainder = testGraph.drawGridLines();

            // Draw Labels
            (double increment, List<double> minLabels) = testGraph.drawLabels();

            // calculate minX and maxX of Graph; resi => [+x, -x, +y,-y]
            List<double> resi = mathOperator.CalculateMinAndMax(remainder, increment, minLabels);

            double step = 0.1;
            double scaleFactor = Math.Abs(testGraph.baseInterval * testGraph.zoomLevel);

            string area;

            // If the request was to Integrate f(x)
            if (integration_)
            {
                // Plot f(x)
                mathOperator.PlotGraph(plotTokens, resi, LexerParser.getNumeric(start_), LexerParser.getNumeric(stop_), step, scaleFactor);
                // Plot ~f(x); returns area under f(x)
                area = mathOperator.PlotIntegral(integral, resi, LexerParser.getNumeric(start_), LexerParser.getNumeric(stop_), step, scaleFactor);
                // Shade the area under f(x)
                mathOperator.ShadeAreaUnderGraph(plotTokens, resi, LexerParser.getNumeric(start_), LexerParser.getNumeric(stop_), scaleFactor);
                // Print Area to info card
                Info_roots.Text = "Area: " + area + "\n";

            }
            // If the request was to plot f(x)
            else if (plot_)
            {
                // Plot f(x)
                mathOperator.PlotGraph(plotTokens, resi, LexerParser.getNumeric(start_), LexerParser.getNumeric(stop_), step, scaleFactor);
                // Plot f'(x)
                mathOperator.PlotDerivative(derivative, resi, step, scaleFactor);

                // Find roots of Polynomial
                double maxIteration = 1000;
                FSharpList<double> roots = LexerParser.newtonMethod(plotTokens, derivative, ListModule.OfSeq(resi), maxIteration, 0.001);

                // Mark roots on the graph
                mathOperator.MarkRoots(roots, scaleFactor);

                // Print roots to Info card
                Info_roots.Text = "Roots: " + roots + "\n";
            }
        }

    }

}
