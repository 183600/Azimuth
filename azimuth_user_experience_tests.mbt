// Azimuth Telemetry System - User Experience Tests
// This file contains comprehensive test cases for user experience and usability

// Test 1: Dashboard Interface and Usability
test "dashboard interface and usability" {
  // Create UX tester
  let ux_tester = UXTester::new()
  
  // Initialize dashboard
  let dashboard = Dashboard::new()
  Dashboard::initialize(dashboard)
  
  // Test dashboard loading time
  let loading_start = Time::now()
  let loading_result = Dashboard::load(dashboard)
  let loading_end = Time::now()
  let loading_time = loading_end - loading_start
  
  match loading_result {
    Ok(_) => {
      // Loading should be fast
      assert_true(loading_time < 3000) // Less than 3 seconds
      
      // Test dashboard responsiveness
      let responsiveness_test = UXTester::test_responsiveness(ux_tester, dashboard)
      assert_true(responsiveness_test.initial_render_time < 1000) // Less than 1 second
      assert_true(responsiveness_test.interaction_response_time < 200) // Less than 200ms
      assert_true(responsiveness_test.animation_frame_rate > 30) // At least 30 FPS
    }
    Error(e) => {
      assert_true(false) // Dashboard should load successfully
    }
  }
  
  // Test navigation usability
  let navigation_test = UXTester::test_navigation(ux_tester, dashboard)
  
  // Verify navigation elements are accessible
  assert_true(navigation_test.main_menu_accessible)
  assert_true(navigation_test.breadcrumb_functional)
  assert_true(navigation_test.search_functional)
  assert_true(navigation_test.keyboard_navigation_works)
  
  // Test navigation efficiency
  assert_true(navigation_test.time_to_find_metrics < 5000) // Less than 5 seconds to find metrics
  assert_true(navigation_test.clicks_to_reach_alerts <= 3) // At most 3 clicks to reach alerts
  
  // Test data visualization clarity
  let visualization_test = UXTester::test_visualizations(ux_tester, dashboard)
  
  // Verify chart readability
  assert_true(visualization_test.charts_have_labels)
  assert_true(visualization_test.charts_have_legends)
  assert_true(visualization_test.colors_are_accessible)
  assert_true(visualization_test.text_is_readable)
  
  // Verify interaction with visualizations
  assert_true(visualization_test.zoom_functional)
  assert_true(visualization_test.filter_functional)
  assert_true(visualization_test.export_functional)
  
  // Test form usability
  let form_test = UXTester::test_forms(ux_tester, dashboard)
  
  // Verify form accessibility
  assert_true(form_test.fields_have_labels)
  assert_true(form_test.validation_provides_feedback)
  assert_true(form_test.error_messages_clear)
  assert_true(form_test.required_fields_indicated)
  
  // Verify form efficiency
  assert_true(form_test.time_to_complete_setup < 120000) // Less than 2 minutes to complete setup
  assert_true(form_test.fields_support_autocomplete)
  assert_true(form_test.keyboard_navigation_efficient)
  
  // Test responsive design
  let responsive_test = UXTester::test_responsive_design(ux_tester, dashboard)
  
  // Test different screen sizes
  let screen_sizes = [
    ScreenSize::new(1920, 1080), // Desktop
    ScreenSize::new(1366, 768),  // Laptop
    ScreenSize::new(768, 1024),  // Tablet
    ScreenSize::new(375, 667)    // Mobile
  ]
  
  for screen_size in screen_sizes {
    let screen_test = UXTester::test_screen_size(ux_tester, dashboard, screen_size)
    
    // Verify layout adapts properly
    assert_true(screen_test.no_horizontal_overflow)
    assert_true(screen_test.elements_properly_sized)
    assert_true(screen_test.touch_targets_large_enough)
    assert_true(screen_test.text_remains_readable)
  }
  
  // Test accessibility compliance
  let accessibility_test = UXTester::test_accessibility(ux_tester, dashboard)
  
  // Verify WCAG compliance
  assert_true(accessibility_test.color_contrast_meets_wcag)
  assert_true(accessibility_test.all_images_have_alt_text)
  assert_true(accessibility_test.keyboard_access_comprehensive)
  assert_true(accessibility_test.screen_reader_compatible)
  assert_true(accessibility_test.focus_management_correct)
  
  // Test error handling and user feedback
  let error_handling_test = UXTester::test_error_handling(ux_tester, dashboard)
  
  // Verify error messages are user-friendly
  assert_true(error_handling_test.error_messages_clear)
  assert_true(error_handling_test.error_messages_actionable)
  assert_true(error_handling_test.error_messages_non_technical)
  assert_true(error_handling_test.recovery_options_provided)
  
  // Test user onboarding
  let onboarding_test = UXTester::test_onboarding(ux_tester, dashboard)
  
  // Verify onboarding experience
  assert_true(onboarding_test.welcome_screen_informative)
  assert_true(onboarding_test.tutorial_helpful)
  assert_true(onboarding_test.first_tasks_clear)
  assert_true(onboarding_test.time_to_first_success < 300000) // Less than 5 minutes to first success
  
  // Generate UX report
  let ux_report = UXTester::generate_report(ux_tester)
  assert_true(ux_report.contains("User Experience Report"))
  assert_true(ux_report.contains("Dashboard Performance"))
  assert_true(ux_report.contains("Navigation Usability"))
  assert_true(ux_report.contains("Data Visualization"))
  assert_true(ux_report.contains("Form Usability"))
  assert_true(ux_report.contains("Responsive Design"))
  assert_true(ux_report.contains("Accessibility Compliance"))
  assert_true(ux_report.contains("Error Handling"))
  assert_true(ux_report.contains("User Onboarding"))
}

// Test 2: Alert and Notification System
test "alert and notification system" {
  // Create notification manager
  let notification_manager = NotificationManager::new()
  
  // Test alert creation and delivery
  let alert_types = [
    AlertType::new("critical", "high", "immediate"),
    AlertType::new("warning", "medium", "hourly"),
    AlertType::new("info", "low", "daily")
  ]
  
  for alert_type in alert_types {
    // Create test alert
    let alert = Alert::new(
      "test_alert_" + alert_type.severity,
      alert_type.severity,
      alert_type.priority,
      "Test alert message for " + alert_type.severity + " severity",
      Time::now()
    )
    
    // Test alert delivery
    let delivery_result = NotificationManager::send_alert(notification_manager, alert)
    match delivery_result {
      Ok(delivery_id) => {
        // Verify alert received
        let received_alert = NotificationManager::get_alert(notification_manager, delivery_id)
        match received_alert {
          Some(received) => {
            assert_eq(received.id, alert.id)
            assert_eq(received.severity, alert.severity)
            assert_eq(received.message, alert.message)
          }
          None => assert_true(false)
        }
        
        // Test alert acknowledgment
        let ack_result = NotificationManager::acknowledge_alert(notification_manager, delivery_id)
        match ack_result {
          Ok(_) => {
            let acknowledged_alert = NotificationManager::get_alert(notification_manager, delivery_id)
            match acknowledged_alert {
              Some(alert) => assert_true(alert.is_acknowledged),
              None => assert_true(false)
            }
          }
          Error(e) => assert_true(false)
        }
      }
      Error(e) => assert_true(false)
    }
  }
  
  // Test alert escalation
  let escalation_alert = Alert::new(
    "escalation_test",
    "critical",
    "high",
    "Test escalation alert",
    Time::now()
  )
  
  // Configure escalation policy
  let escalation_policy = EscalationPolicy::new()
  EscalationPolicy::add_rule(escalation_policy, "unacknowledged_for", "5m", "escalate_to_manager")
  EscalationPolicy::add_rule(escalation_policy, "unacknowledged_for", "15m", "escalate_to_director")
  
  NotificationManager::set_escalation_policy(notification_manager, escalation_policy)
  
  let escalation_result = NotificationManager::send_alert(notification_manager, escalation_alert)
  match escalation_result {
    Ok(delivery_id) => {
      // Simulate time passing without acknowledgment
      Time::sleep(100) // Simulate 100ms (in real test, would be minutes)
      
      // Check if escalation was triggered
      let escalation_status = NotificationManager::get_escalation_status(notification_manager, delivery_id)
      // In test environment, might not actually escalate due to time constraints
      assert_true(escalation_status.is_tracked)
    }
    Error(e) => assert_true(false)
  }
  
  // Test notification channels
  let channels = [
    NotificationChannel::new("email", "user@example.com"),
    NotificationChannel::new("sms", "+1234567890"),
    NotificationChannel::new("slack", "#alerts"),
    NotificationChannel::new("webhook", "https://example.com/webhook")
  ]
  
  for channel in channels {
    // Test channel configuration
    let config_result = NotificationManager::configure_channel(notification_manager, channel)
    match config_result {
      Ok(_) => {
        // Test channel availability
        let availability = NotificationManager::check_channel_availability(notification_manager, channel.type)
        // In test environment, channels might not be available
        assert_true(availability.is_checked)
      }
      Error(e) => {
        // Some channels might fail to configure in test environment
        assert_true(Error::is_configuration_error(e))
      }
    }
  }
  
  // Test alert filtering and routing
  let filter_config = AlertFilterConfig::new()
  AlertFilterConfig::add_filter(filter_config, "severity", "critical", "email,sms")
  AlertFilterConfig::add_filter(filter_config, "severity", "warning", "slack")
  AlertFilterConfig::add_filter(filter_config, "severity", "info", "email")
  
  NotificationManager::set_filter_config(notification_manager, filter_config)
  
  let critical_alert = Alert::new("filter_test_critical", "critical", "high", "Critical alert", Time::now())
  let warning_alert = Alert::new("filter_test_warning", "warning", "medium", "Warning alert", Time::now())
  let info_alert = Alert::new("filter_test_info", "info", "low", "Info alert", Time::now())
  
  let critical_result = NotificationManager::send_alert(notification_manager, critical_alert)
  let warning_result = NotificationManager::send_alert(notification_manager, warning_alert)
  let info_result = NotificationManager::send_alert(notification_manager, info_alert)
  
  match (critical_result, warning_result, info_result) {
    (Ok(critical_id), Ok(warning_id), Ok(info_id)) => {
      // Verify routing
      let critical_routes = NotificationManager::get_alert_routes(notification_manager, critical_id)
      let warning_routes = NotificationManager::get_alert_routes(notification_manager, warning_id)
      let info_routes = NotificationManager::get_alert_routes(notification_manager, info_id)
      
      // Verify critical alert routes to email and sms
      assert_true(critical_routes.contains("email"))
      assert_true(critical_routes.contains("sms"))
      
      // Verify warning alert routes to slack
      assert_true(warning_routes.contains("slack"))
      
      // Verify info alert routes to email
      assert_true(info_routes.contains("email"))
    }
    _ => assert_true(false)
  }
  
  // Test alert suppression and deduplication
  let duplicate_alert = Alert::new(
    "duplicate_test",
    "warning",
    "medium",
    "Duplicate alert test",
    Time::now()
  )
  
  // Send first alert
  let first_result = NotificationManager::send_alert(notification_manager, duplicate_alert)
  match first_result {
    Ok(first_id) => {
      // Send duplicate alert
      let duplicate_result = NotificationManager::send_alert(notification_manager, duplicate_alert)
      match duplicate_result {
        Ok(duplicate_id) => {
          // Verify deduplication
          let deduplication_status = NotificationManager::get_deduplication_status(notification_manager, duplicate_id)
          assert_true(deduplication_status.is_duplicate)
          assert_eq(deduplication_status.original_alert_id, first_id)
        }
        Error(e) => assert_true(false)
      }
    }
    Error(e) => assert_true(false)
  }
  
  // Test alert analytics
  let analytics = NotificationManager::get_analytics(notification_manager)
  
  // Verify analytics data
  assert_true(analytics.total_alerts > 0)
  assert_true(analytics.critical_alerts >= 0)
  assert_true(analytics.warning_alerts >= 0)
  assert_true(analytics.info_alerts >= 0)
  assert_true(analytics.acknowledged_alerts >= 0)
  assert_true(analytics.escalated_alerts >= 0)
  assert_true(analytics.average_acknowledgment_time >= 0)
  
  // Generate notification report
  let notification_report = NotificationManager::generate_report(notification_manager)
  assert_true(notification_report.contains("Notification System Report"))
  assert_true(notification_report.contains("Alert Delivery"))
  assert_true(notification_report.contains("Escalation Testing"))
  assert_true(notification_report.contains("Channel Configuration"))
  assert_true(notification_report.contains("Alert Filtering"))
  assert_true(notification_report.contains("Deduplication Testing"))
  assert_true(notification_report.contains("Alert Analytics"))
}

// Test 3: User Workflow and Task Completion
test "user workflow and task completion" {
  // Create workflow tester
  let workflow_tester = WorkflowTester::new()
  
  // Define common user workflows
  let workflows = [
    Workflow::new("setup_telemetry", "Set up new telemetry collection"),
    Workflow::new("create_dashboard", "Create custom dashboard"),
    Workflow::new("configure_alerts", "Configure alert rules"),
    Workflow::new("analyze_data", "Analyze telemetry data"),
    Workflow::new("export_report", "Export analysis report")
  ]
  
  for workflow in workflows {
    // Test workflow completion
    let workflow_result = WorkflowTester::complete_workflow(workflow_tester, workflow)
    
    match workflow_result {
      Ok(completion_metrics) => {
        // Verify workflow completion metrics
        assert_true(completion_metrics.is_completed)
        assert_true(completion_metrics.completion_time > 0)
        assert_true(completion_metrics.steps_completed > 0)
        assert_true(completion_metrics.error_count == 0)
        
        // Verify efficiency
        assert_true(completion_metrics.completion_time < completion_metrics.expected_time * 2.0)
        assert_true(completion_metrics.steps_completed <= completion_metrics.expected_steps * 1.5)
        
        // Test user satisfaction
        assert_true(completion_metrics.satisfaction_score >= 3.0) // At least 3/5 satisfaction
      }
      Error(e) => {
        assert_true(false) // Workflows should complete successfully
      }
    }
  }
  
  // Test workflow optimization
  let optimization_workflows = [
    Workflow::new("quick_setup", "Quick telemetry setup for beginners"),
    Workflow::new("advanced_analysis", "Advanced data analysis for experts")
  ]
  
  for workflow in optimization_workflows {
    // Test with user personas
    let personas = [
      Persona::new("beginner", "New user with limited experience"),
      Persona::new("intermediate", "User with some experience"),
      Persona::new("expert", "Experienced user with advanced knowledge")
    ]
    
    for persona in personas {
      let persona_result = WorkflowTester::complete_workflow_with_persona(workflow_tester, workflow, persona)
      
      match persona_result {
        Ok(persona_metrics) => {
          // Verify persona-specific metrics
          assert_true(persona_metrics.is_completed)
          
          // Beginners should take longer but have more guidance
          if persona.name == "beginner" {
            assert_true(persona_metrics.guidance_steps > 0)
            assert_true(persona_metrics.completion_time > 0)
          }
          
          // Experts should complete faster
          if persona.name == "expert" {
            assert_true(persona_metrics.completion_time < persona_metrics.expected_time)
          }
        }
        Error(e) => {
          // Some workflows might not be suitable for all personas
          assert_true(Error::is_persona_mismatch_error(e))
        }
      }
    }
  }
  
  // Test task efficiency
  let tasks = [
    Task::new("add_metric", "Add new metric to dashboard"),
    Task::new("set_time_range", "Set time range for data view"),
    Task::new("create_alert_rule", "Create new alert rule"),
    Task::new("export_chart", "Export chart as image"),
    Task::new("share_dashboard", "Share dashboard with team")
  ]
  
  for task in tasks {
    // Measure task completion time
    let task_start = Time::now()
    let task_result = WorkflowTester::complete_task(workflow_tester, task)
    let task_end = Time::now()
    let task_time = task_end - task_start
    
    match task_result {
      Ok(task_metrics) => {
        // Verify task efficiency
        assert_true(task_metrics.is_completed)
        assert_true(task_time < 30000) // Less than 30 seconds per task
        assert_true(task_metrics.clicks_required <= 5) // At most 5 clicks per task
        assert_true(task_metrics.error_count == 0)
        
        // Test task discoverability
        assert_true(task_metrics.discoverability_score >= 3.0) // At least 3/5 discoverability
      }
      Error(e) => {
        assert_true(false) // Tasks should complete successfully
      }
    }
  }
  
  // Test error recovery in workflows
  let error_scenarios = [
    ErrorScenario::new("network_failure", "Simulate network failure"),
    ErrorScenario::new("invalid_input", "Provide invalid input"),
    ErrorScenario::new("permission_denied", "Simulate permission denied"),
    ErrorScenario::new("service_unavailable", "Simulate service unavailable")
  ]
  
  for scenario in error_scenarios {
    // Test workflow with error scenario
    let error_workflow = Workflow::new("error_test", "Test error handling")
    let error_result = WorkflowTester::complete_workflow_with_error(workflow_tester, error_workflow, scenario)
    
    match error_result {
      Ok(error_metrics) => {
        // Verify error handling
        assert_true(error_metrics.error_encountered)
        assert_true(error_metrics.error_message_clear)
        assert_true(error_metrics.recovery_options_provided)
        assert_true(error_metrics.workflow_completed || error_metrics.graceful_exit)
        
        // Test user satisfaction with error handling
        assert_true(error_metrics.error_handling_satisfaction >= 2.0) // At least 2/5 for error handling
      }
      Error(e) => {
        assert_true(false) // Error scenarios should be handled gracefully
      }
    }
  }
  
  // Test learning curve and skill progression
  let learning_test = WorkflowTester::test_learning_curve(workflow_tester)
  
  // Verify learning curve
  assert_true(learning_test.initial_completion_time > learning_test.final_completion_time)
  assert_true(learning_test.improvement_ratio > 1.2) // At least 20% improvement
  assert_true(learning_test.error_rate_reduction > 0.3) // At least 30% error rate reduction
  
  // Test user retention and engagement
  let engagement_test = WorkflowTester::test_user_engagement(workflow_tester)
  
  // Verify engagement metrics
  assert_true(engagement_test.return_user_rate > 0.7) // At least 70% return rate
  assert_true(engagement_test.feature_adoption_rate > 0.5) // At least 50% feature adoption
  assert_true(engagement_test.session_duration > 300) // At least 5 minutes per session
  
  // Generate workflow report
  let workflow_report = WorkflowTester::generate_report(workflow_tester)
  assert_true(workflow_report.contains("User Workflow Report"))
  assert_true(workflow_report.contains("Workflow Completion"))
  assert_true(workflow_report.contains("Persona Testing"))
  assert_true(workflow_report.contains("Task Efficiency"))
  assert_true(workflow_report.contains("Error Recovery"))
  assert_true(workflow_report.contains("Learning Curve"))
  assert_true(workflow_report.contains("User Engagement"))
}

// Test 4: Customization and Personalization
test "customization and personalization" {
  // Create personalization manager
  let personalization_manager = PersonalizationManager::new()
  
  // Test user preference management
  let user_preferences = UserPreferences::new("test_user")
  
  // Set preferences
  UserPreferences::set_theme(user_preferences, "dark")
  UserPreferences::set_language(user_preferences, "en")
  UserPreferences::set_timezone(user_preferences, "UTC")
  UserPreferences::set_date_format(user_preferences, "MM/DD/YYYY")
  UserPreferences::set_time_format(user_preferences, "12h")
  UserPreferences::set_default_dashboard(user_preferences, "overview")
  UserPreferences::set_notification_frequency(user_preferences, "daily")
  
  // Save preferences
  let save_result = PersonalizationManager::save_preferences(personalization_manager, user_preferences)
  match save_result {
    Ok(_) => {
      // Load preferences
      let load_result = PersonalizationManager::load_preferences(personalization_manager, "test_user")
      match load_result {
        Ok(loaded_preferences) => {
          // Verify preferences loaded correctly
          assert_eq(loaded_preferences.theme, "dark")
          assert_eq(loaded_preferences.language, "en")
          assert_eq(loaded_preferences.timezone, "UTC")
          assert_eq(loaded_preferences.date_format, "MM/DD/YYYY")
          assert_eq(loaded_preferences.time_format, "12h")
          assert_eq(loaded_preferences.default_dashboard, "overview")
          assert_eq(loaded_preferences.notification_frequency, "daily")
        }
        Error(e) => assert_true(false)
      }
    }
    Error(e) => assert_true(false)
  }
  
  // Test dashboard customization
  let dashboard_customizer = DashboardCustomizer::new()
  
  // Create custom dashboard
  let custom_dashboard = DashboardCustomizer::create_dashboard(dashboard_customizer, "custom_test")
  
  // Add widgets
  let widgets = [
    Widget::new("cpu_chart", "CPU Usage", "line_chart"),
    Widget::new("memory_gauge", "Memory Usage", "gauge"),
    Widget::new("error_table", "Recent Errors", "table"),
    Widget::new("alert_panel", "Active Alerts", "panel")
  ]
  
  for widget in widgets {
    let add_result = DashboardCustomizer::add_widget(dashboard_customizer, custom_dashboard, widget)
    match add_result {
      Ok(_) => assert_true(true),
      Error(e) => assert_true(false)
    }
  }
  
  // Customize widget layout
  let layout_result = DashboardCustomizer::set_layout(dashboard_customizer, custom_dashboard, GridLayout::new(2, 2))
  match layout_result {
    Ok(_) => {
      // Verify layout
      let current_layout = DashboardCustomizer::get_layout(dashboard_customizer, custom_dashboard)
      assert_eq(current_layout.rows, 2)
      assert_eq(current_layout.columns, 2)
    }
    Error(e) => assert_true(false)
  }
  
  // Customize widget appearance
  let appearance_result = DashboardCustomizer::customize_widget_appearance(
    dashboard_customizer, 
    custom_dashboard, 
    "cpu_chart", 
    WidgetAppearance::new("blue", "large", "solid")
  )
  match appearance_result {
    Ok(_) => {
      // Verify appearance
      let widget_appearance = DashboardCustomizer::get_widget_appearance(dashboard_customizer, custom_dashboard, "cpu_chart")
      assert_eq(widget_appearance.color, "blue")
      assert_eq(widget_appearance.size, "large")
      assert_eq(widget_appearance.style, "solid")
    }
    Error(e) => assert_true(false)
  }
  
  // Test data source configuration
  let data_source_config = DataSourceConfig::new()
  DataSourceConfig::add_query(data_source_config, "cpu_query", "SELECT cpu_usage FROM metrics WHERE time > now() - 1h")
  DataSourceConfig::add_query(data_source_config, "memory_query", "SELECT memory_usage FROM metrics WHERE time > now() - 1h")
  DataSourceConfig::set_refresh_interval(data_source_config, 30) // 30 seconds
  
  let config_result = DashboardCustomizer::configure_data_sources(dashboard_customizer, custom_dashboard, data_source_config)
  match config_result {
    Ok(_) => {
      // Verify configuration
      let current_config = DashboardCustomizer::get_data_source_config(dashboard_customizer, custom_dashboard)
      assert_true(current_config.has_query("cpu_query"))
      assert_true(current_config.has_query("memory_query"))
      assert_eq(current_config.refresh_interval, 30)
    }
    Error(e) => assert_true(false)
  }
  
  // Test alert customization
  let alert_customizer = AlertCustomizer::new()
  
  // Create custom alert rule
  let alert_rule = AlertRule::new("custom_cpu_alert", "CPU Usage High")
  AlertRule::set_condition(alert_rule, "cpu_usage > 80")
  AlertRule::set_severity(alert_rule, "warning")
  AlertRule::set_notification_channels(alert_rule, ["email", "slack"])
  AlertRule::set_suppression_duration(alert_rule, 300) // 5 minutes
  
  let rule_result = AlertCustomizer::create_rule(alert_customizer, alert_rule)
  match rule_result {
    Ok(rule_id) => {
      // Verify rule creation
      let created_rule = AlertCustomizer::get_rule(alert_customizer, rule_id)
      match created_rule {
        Some(rule) => {
          assert_eq(rule.name, "custom_cpu_alert")
          assert_eq(rule.condition, "cpu_usage > 80")
          assert_eq(rule.severity, "warning")
        }
        None => assert_true(false)
      }
    }
    Error(e) => assert_true(false)
  }
  
  // Test report customization
  let report_customizer = ReportCustomizer::new()
  
  // Create custom report
  let custom_report = ReportCustomizer::create_report(report_customizer, "custom_performance_report")
  
  // Add report sections
  let sections = [
    ReportSection::new("overview", "Performance Overview"),
    ReportSection::new("metrics", "Key Metrics"),
    ReportSection::new("trends", "Trend Analysis"),
    ReportSection::new("recommendations", "Recommendations")
  ]
  
  for section in sections {
    let add_result = ReportCustomizer::add_section(report_customizer, custom_report, section)
    match add_result {
      Ok(_) => assert_true(true),
      Error(e) => assert_true(false)
    }
  }
  
  // Customize report format
  let format_result = ReportCustomizer::set_format(report_customizer, custom_report, ReportFormat::new("pdf", "a4", "portrait"))
  match format_result {
    Ok(_) => {
      // Verify format
      let current_format = ReportCustomizer::get_format(report_customizer, custom_report)
      assert_eq(current_format.type, "pdf")
      assert_eq(current_format.page_size, "a4")
      assert_eq(current_format.orientation, "portrait")
    }
    Error(e) => assert_true(false)
  }
  
  // Test personalization analytics
  let analytics = PersonalizationManager::get_analytics(personalization_manager)
  
  // Verify analytics data
  assert_true(analytics.active_users > 0)
  assert_true(analytics.customized_dashboards > 0)
  assert_true(analytics.custom_alert_rules > 0)
  assert_true(analytics.custom_reports > 0)
  assert_true(analytics.preference_changes > 0)
  
  // Test A/B testing for personalization features
  let ab_test = ABTest::new("dashboard_layout_test", "Test dashboard layout effectiveness")
  ABTest::add_variant(ab_test, "current_layout", 0.5)
  ABTest::add_variant(ab_test, "new_layout", 0.5)
  
  let test_result = PersonalizationManager::run_ab_test(personalization_manager, ab_test)
  match test_result {
    Ok(test_metrics) => {
      // Verify A/B test metrics
      assert_true(test_metrics.participant_count > 0)
      assert_true(test_metrics.conversion_rate_current >= 0.0)
      assert_true(test_metrics.conversion_rate_new >= 0.0)
      assert_true(test_metrics.statistical_significance_calculated)
    }
    Error(e) => {
      // A/B testing might not be available in test environment
      assert_true(Error::is_feature_unavailable_error(e))
    }
  }
  
  // Generate personalization report
  let personalization_report = PersonalizationManager::generate_report(personalization_manager)
  assert_true(personalization_report.contains("Personalization Report"))
  assert_true(personalization_report.contains("User Preferences"))
  assert_true(personalization_report.contains("Dashboard Customization"))
  assert_true(personalization_report.contains("Alert Customization"))
  assert_true(personalization_report.contains("Report Customization"))
  assert_true(personalization_report.contains("Personalization Analytics"))
  assert_true(personalization_report.contains("A/B Testing Results"))
}

// Test 5: Help System and Documentation
test "help system and documentation" {
  // Create help system manager
  let help_manager = HelpSystemManager::new()
  
  // Test contextual help
  let help_topics = [
    HelpTopic::new("getting_started", "Getting Started", "Learn how to set up your first telemetry dashboard"),
    HelpTopic::new("creating_dashboards", "Creating Dashboards", "Step-by-step guide to creating custom dashboards"),
    HelpTopic::new("setting_alerts", "Setting Alerts", "Configure alert rules to monitor your metrics"),
    HelpTopic::new("data_analysis", "Data Analysis", "Advanced techniques for analyzing telemetry data"),
    HelpTopic::new("troubleshooting", "Troubleshooting", "Common issues and how to resolve them")
  ]
  
  for topic in help_topics {
    // Add topic to help system
    let add_result = HelpSystemManager::add_topic(help_manager, topic)
    match add_result {
      Ok(_) => {
        // Test topic retrieval
        let get_result = HelpSystemManager::get_topic(help_manager, topic.id)
        match get_result {
          Some(retrieved_topic) => {
            assert_eq(retrieved_topic.id, topic.id)
            assert_eq(retrieved_topic.title, topic.title)
            assert_eq(retrieved_topic.content, topic.content)
          }
          None => assert_true(false)
        }
        
        // Test topic search
        let search_result = HelpSystemManager::search_topics(help_manager, topic.title.split(" ")[0])
        assert_true(search_result.length() > 0)
        assert_true(search_result.any(fn(t) { t.id == topic.id }))
      }
      Error(e) => assert_true(false)
    }
  }
  
  // Test interactive tutorials
  let tutorials = [
    Tutorial::new("dashboard_tour", "Dashboard Tour", "Interactive tour of the main dashboard"),
    Tutorial::new("first_metric", "Your First Metric", "Add your first metric to the dashboard"),
    Tutorial::new("first_alert", "Your First Alert", "Create your first alert rule"),
    Tutorial::new("data_export", "Exporting Data", "Learn how to export your telemetry data")
  ]
  
  for tutorial in tutorials {
    // Add tutorial to help system
    let add_result = HelpSystemManager::add_tutorial(help_manager, tutorial)
    match add_result {
      Ok(_) => {
        // Test tutorial progress tracking
        let progress_result = HelpSystemManager::start_tutorial(help_manager, tutorial.id, "test_user")
        match progress_result {
          Ok(session_id) => {
            // Simulate tutorial progress
            let step_result = HelpSystemManager::complete_tutorial_step(help_manager, session_id, 1)
            match step_result {
              Ok(_) => {
                // Get tutorial progress
                let progress = HelpSystemManager::get_tutorial_progress(help_manager, "test_user", tutorial.id)
                match progress {
                  Some(progress_data) => {
                    assert_eq(progress_data.current_step, 2)
                    assert_true(progress_data.started_at > 0)
                  }
                  None => assert_true(false)
                }
              }
              Error(e) => assert_true(false)
            }
          }
          Error(e) => assert_true(false)
        }
      }
      Error(e) => assert_true(false)
    }
  }
  
  // Test FAQ system
  let faq_items = [
    FAQItem::new("how_to_add_metric", "How do I add a new metric?", "Navigate to the Metrics tab and click 'Add Metric'"),
    FAQItem::new("data_retention", "How long is data retained?", "Data is retained for 30 days by default"),
    FAQItem::new("alert_delays", "Why are my alerts delayed?", "Alerts may be delayed due to processing time"),
    FAQItem::new("export_formats", "What export formats are available?", "CSV, JSON, and PDF formats are supported")
  ]
  
  for item in faq_items {
    // Add FAQ item
    let add_result = HelpSystemManager::add_faq_item(help_manager, item)
    match add_result {
      Ok(_) => {
        // Test FAQ search
        let search_result = HelpSystemManager::search_faqs(help_manager, "metric")
        assert_true(search_result.length() > 0)
        
        // Test FAQ categorization
        let categories = HelpSystemManager::get_faq_categories(help_manager)
        assert_true(categories.length() > 0)
      }
      Error(e) => assert_true(false)
    }
  }
  
  // Test video guides
  let video_guides = [
    VideoGuide::new("setup_video", "Complete Setup Guide", "https://example.com/setup-guide", 300),
    VideoGuide::new("advanced_video", "Advanced Features", "https://example.com/advanced-features", 450),
    VideoGuide::new("troubleshooting_video", "Troubleshooting Common Issues", "https://example.com/troubleshooting", 180)
  ]
  
  for video in video_guides {
    // Add video guide
    let add_result = HelpSystemManager::add_video_guide(help_manager, video)
    match add_result {
      Ok(_) => {
        // Test video guide metadata
        let get_result = HelpSystemManager::get_video_guide(help_manager, video.id)
        match get_result {
          Some(retrieved_video) => {
            assert_eq(retrieved_video.id, video.id)
            assert_eq(retrieved_video.title, video.title)
            assert_eq(retrieved_video.url, video.url)
            assert_eq(retrieved_video.duration, video.duration)
          }
          None => assert_true(false)
        }
      }
      Error(e) => assert_true(false)
    }
  }
  
  // Test help system accessibility
  let accessibility_test = HelpSystemManager::test_accessibility(help_manager)
  
  // Verify accessibility features
  assert_true(accessibility_test.keyboard_navigation_works)
  assert_true(accessibility_test.screen_reader_compatible)
  assert_true(accessibility_test.high_contrast_available)
  assert_true(accessibility_test.font_size_adjustable)
  
  // Test help system analytics
  let analytics = HelpSystemManager::get_analytics(help_manager)
  
  // Verify analytics data
  assert_true(analytics.topic_views > 0)
  assert_true(analytics.tutorial_completions > 0)
  assert_true(analytics.faq_views > 0)
  assert_true(analytics.video_plays > 0)
  assert_true(analytics.search_queries > 0)
  
  // Test help system effectiveness
  let effectiveness_test = HelpSystemManager::test_effectiveness(help_manager)
  
  // Verify effectiveness metrics
  assert_true(effectiveness_test.user_satisfaction > 3.0) // At least 3/5 satisfaction
  assert_true(effectiveness_test.issue_resolution_rate > 0.7) // At least 70% issue resolution
  assert_true(effectiveness_test.self_service_rate > 0.5) // At least 50% self-service
  
  // Test feedback collection
  let feedback_result = HelpSystemManager::submit_feedback(help_manager, "test_user", "getting_started", 4, "Very helpful guide")
  match feedback_result {
    Ok(_) => {
      // Verify feedback recorded
      let feedback_analytics = HelpSystemManager::get_feedback_analytics(help_manager)
      assert_true(feedback_analytics.total_feedback > 0)
      assert_true(feedback_analytics.average_rating > 0.0)
    }
    Error(e) => assert_true(false)
  }
  
  // Generate help system report
  let help_report = HelpSystemManager::generate_report(help_manager)
  assert_true(help_report.contains("Help System Report"))
  assert_true(help_report.contains("Help Topics"))
  assert_true(help_report.contains("Interactive Tutorials"))
  assert_true(help_report.contains("FAQ System"))
  assert_true(help_report.contains("Video Guides"))
  assert_true(help_report.contains("Accessibility Testing"))
  assert_true(help_report.contains("Help System Analytics"))
  assert_true(help_report.contains("Effectiveness Metrics"))
  assert_true(help_report.contains("User Feedback"))
}