using System.Windows;
using System.Windows.Documents;
using FClassLibrary;

namespace WpfPathfinder
{
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
            LexerParser.terminal lexed = LexerParser.lexer(input);
            cmdWindow.AppendText("> " + LexerParser.parser(lexed) + "\n");
            float answer = LexerParser.parseNeval(lexed);
            cmdWindow.AppendText("> " + answer + "\n");
            Input.Clear();

        }
    }
}
