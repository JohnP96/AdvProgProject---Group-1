﻿#pragma checksum "..\..\..\MainWindow.xaml" "{ff1816ec-aa5e-4d10-87f7-6f4963833460}" "CDB9CAE31DE876DE07B72A98690EF6F0CC9912F9"
//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     Runtime Version:4.0.30319.42000
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

using System;
using System.Diagnostics;
using System.Windows;
using System.Windows.Automation;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using System.Windows.Controls.Ribbon;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Ink;
using System.Windows.Input;
using System.Windows.Markup;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Media.Effects;
using System.Windows.Media.Imaging;
using System.Windows.Media.Media3D;
using System.Windows.Media.TextFormatting;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Windows.Shell;


namespace InterpreterWPF {
    
    
    /// <summary>
    /// MainWindow
    /// </summary>
    public partial class MainWindow : System.Windows.Window, System.Windows.Markup.IComponentConnector {
        
        
        #line 32 "..\..\..\MainWindow.xaml"
        [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
        internal System.Windows.Controls.TextBox cmdWindow;
        
        #line default
        #line hidden
        
        
        #line 40 "..\..\..\MainWindow.xaml"
        [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
        internal System.Windows.Controls.TextBox Input;
        
        #line default
        #line hidden
        
        
        #line 41 "..\..\..\MainWindow.xaml"
        [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
        internal System.Windows.Controls.Button enterBtn;
        
        #line default
        #line hidden
        
        
        #line 46 "..\..\..\MainWindow.xaml"
        [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
        internal System.Windows.Controls.TextBox VariableTracker;
        
        #line default
        #line hidden
        
        
        #line 50 "..\..\..\MainWindow.xaml"
        [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
        internal System.Windows.Controls.Canvas graphCanvas;
        
        #line default
        #line hidden
        
        private bool _contentLoaded;
        
        /// <summary>
        /// InitializeComponent
        /// </summary>
        [System.Diagnostics.DebuggerNonUserCodeAttribute()]
        [System.CodeDom.Compiler.GeneratedCodeAttribute("PresentationBuildTasks", "7.0.4.0")]
        public void InitializeComponent() {
            if (_contentLoaded) {
                return;
            }
            _contentLoaded = true;
            System.Uri resourceLocater = new System.Uri("/InterpreterWPF;component/mainwindow.xaml", System.UriKind.Relative);
            
            #line 1 "..\..\..\MainWindow.xaml"
            System.Windows.Application.LoadComponent(this, resourceLocater);
            
            #line default
            #line hidden
        }
        
        [System.Diagnostics.DebuggerNonUserCodeAttribute()]
        [System.CodeDom.Compiler.GeneratedCodeAttribute("PresentationBuildTasks", "7.0.4.0")]
        [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Never)]
        [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Design", "CA1033:InterfaceMethodsShouldBeCallableByChildTypes")]
        [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Maintainability", "CA1502:AvoidExcessiveComplexity")]
        [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1800:DoNotCastUnnecessarily")]
        void System.Windows.Markup.IComponentConnector.Connect(int connectionId, object target) {
            switch (connectionId)
            {
            case 1:
            
            #line 8 "..\..\..\MainWindow.xaml"
            ((InterpreterWPF.MainWindow)(target)).Loaded += new System.Windows.RoutedEventHandler(this.DrawGraph);
            
            #line default
            #line hidden
            
            #line 9 "..\..\..\MainWindow.xaml"
            ((InterpreterWPF.MainWindow)(target)).SizeChanged += new System.Windows.SizeChangedEventHandler(this.DrawGraph);
            
            #line default
            #line hidden
            return;
            case 2:
            this.cmdWindow = ((System.Windows.Controls.TextBox)(target));
            return;
            case 3:
            this.Input = ((System.Windows.Controls.TextBox)(target));
            return;
            case 4:
            this.enterBtn = ((System.Windows.Controls.Button)(target));
            
            #line 41 "..\..\..\MainWindow.xaml"
            this.enterBtn.Click += new System.Windows.RoutedEventHandler(this.enterBtn_Click);
            
            #line default
            #line hidden
            return;
            case 5:
            this.VariableTracker = ((System.Windows.Controls.TextBox)(target));
            return;
            case 6:
            this.graphCanvas = ((System.Windows.Controls.Canvas)(target));
            
            #line 52 "..\..\..\MainWindow.xaml"
            this.graphCanvas.MouseWheel += new System.Windows.Input.MouseWheelEventHandler(this.zoom);
            
            #line default
            #line hidden
            
            #line 53 "..\..\..\MainWindow.xaml"
            this.graphCanvas.MouseDown += new System.Windows.Input.MouseButtonEventHandler(this.mouseDown);
            
            #line default
            #line hidden
            
            #line 54 "..\..\..\MainWindow.xaml"
            this.graphCanvas.MouseMove += new System.Windows.Input.MouseEventHandler(this.graphCanvas_MouseMove);
            
            #line default
            #line hidden
            return;
            }
            this._contentLoaded = true;
        }
    }
}

