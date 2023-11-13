﻿using System;
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

    }
}
