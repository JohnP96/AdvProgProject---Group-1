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
            //cmdWindow.AppendText("> " + LexerParser.parser(lexed) + "\n");
            int answer = LexerParser.parseNeval(lexed).Item2;
            cmdWindow.AppendText("> " + answer + "\n");
            Input.Clear();
        }

    }
}
