using System.Windows;

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
            Input.Clear();
        }
    }
}
