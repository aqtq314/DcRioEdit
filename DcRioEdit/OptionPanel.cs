using DcRioEdit.Models;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;

namespace DcRioEdit
{
    public class OptionPanel : Control
    {
        static OptionPanel()
        {
            DefaultStyleKeyProperty.OverrideMetadata(typeof(OptionPanel), new FrameworkPropertyMetadata(typeof(OptionPanel)));
        }

        public OptionPanel()
        {
            DataContextChanged += (sender, e) =>
            {
                UpdateContentTemplate();
            };
        }

        private static readonly DependencyPropertyKey CurrentContentPropertyKey =
            DependencyProperty.RegisterReadOnly("CurrentContent", typeof(object), typeof(OptionPanel),
                new FrameworkPropertyMetadata(null));

        public static readonly DependencyProperty CurrentContentProperty = CurrentContentPropertyKey.DependencyProperty;

        private static readonly DependencyPropertyKey CurrentContentTemplatePropertyKey =
            DependencyProperty.RegisterReadOnly("CurrentContentTemplate", typeof(DataTemplate), typeof(OptionPanel),
                new FrameworkPropertyMetadata(null));

        public static readonly DependencyProperty CurrentContentTemplateProperty = CurrentContentTemplatePropertyKey.DependencyProperty;

        public static readonly DependencyProperty NoneTemplateProperty =
            DependencyProperty.Register("NoneTemplate", typeof(DataTemplate), typeof(OptionPanel),
                new FrameworkPropertyMetadata(null,
                    (d, e) => ((OptionPanel)d).OnNoneTemplateChanged((DataTemplate)e.OldValue, (DataTemplate)e.NewValue)));

        public static readonly DependencyProperty SomeTemplateProperty =
            DependencyProperty.Register("SomeTemplate", typeof(DataTemplate), typeof(OptionPanel),
                new FrameworkPropertyMetadata(null,
                    (d, e) => ((OptionPanel)d).OnSomeTemplateChanged((DataTemplate)e.OldValue, (DataTemplate)e.NewValue)));

        public static readonly DependencyProperty OptionValueProperty =
            DependencyProperty.Register("OptionValue", typeof(WpfOption<object>), typeof(OptionPanel),
                new FrameworkPropertyMetadata(WpfOptionUtil.WpfNone<object>(),
                    (d, e) => ((OptionPanel)d).OnOptionValueChanged((WpfOption<object>)e.OldValue, (WpfOption<object>)e.NewValue)));

        public object CurrentContent
        {
            get { return (object)GetValue(CurrentContentProperty); }
            protected set { SetValue(CurrentContentPropertyKey, value); }
        }

        public DataTemplate CurrentContentTemplate
        {
            get { return (DataTemplate)GetValue(CurrentContentTemplateProperty); }
            protected set { SetValue(CurrentContentTemplatePropertyKey, value); }
        }

        public DataTemplate NoneTemplate
        {
            get { return (DataTemplate)GetValue(NoneTemplateProperty); }
            set { SetValue(NoneTemplateProperty, value); }
        }

        public DataTemplate SomeTemplate
        {
            get { return (DataTemplate)GetValue(SomeTemplateProperty); }
            set { SetValue(SomeTemplateProperty, value); }
        }

        public WpfOption<object> OptionValue
        {
            get { return (WpfOption<object>)GetValue(OptionValueProperty); }
            set { SetValue(OptionValueProperty, value); }
        }

        private void OnNoneTemplateChanged(DataTemplate oldValue, DataTemplate newValue)
        {
            UpdateContentTemplate();
        }

        private void OnSomeTemplateChanged(DataTemplate oldValue, DataTemplate newValue)
        {
            UpdateContentTemplate();
        }

        private void OnOptionValueChanged(WpfOption<object> oldValue, WpfOption<object> newValue)
        {
            UpdateContentTemplate();
        }

        private void UpdateContentTemplate()
        {
            if (OptionValue.IsSome)
            {
                CurrentContentTemplate = SomeTemplate;
                CurrentContent = OptionValue.Value;
            }
            else
            {
                CurrentContentTemplate = NoneTemplate;
                CurrentContent = DataContext;
            }
        }
    }
}
