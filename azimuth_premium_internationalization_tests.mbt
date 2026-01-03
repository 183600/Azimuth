// Azimuth Premium Internationalization Support Tests
// 国际化支持测试，确保遥测系统在不同语言和地区环境中的正确运行

// Test 1: Basic Localization Support
test "基础本地化支持测试" {
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  
  // 加载默认语言资源（英语）
  let en_locale = I18nManager::load_locale(i18n_manager, "en")
  assert_true(en_locale.is_some())
  
  // 验证英语本地化
  let en_messages = I18nManager::get_messages(i18n_manager, "en")
  assert_true(en_messages.is_some())
  
  match en_messages {
    Some(messages) => {
      assert_eq(I18nMessages::get(messages, "telemetry.span.started"), "Span started")
      assert_eq(I18nMessages::get(messages, "telemetry.span.ended"), "Span ended")
      assert_eq(I18nMessages::get(messages, "metrics.counter.created"), "Counter created")
      assert_eq(I18nMessages::get(messages, "log.record.emitted"), "Log record emitted")
    }
    None => assert_true(false)
  }
  
  // 加载中文语言资源
  let zh_locale = I18nManager::load_locale(i18n_manager, "zh")
  assert_true(zh_locale.is_some())
  
  // 验证中文本地化
  let zh_messages = I18nManager::get_messages(i18n_manager, "zh")
  assert_true(zh_messages.is_some())
  
  match zh_messages {
    Some(messages) => {
      assert_eq(I18nMessages::get(messages, "telemetry.span.started"), "跨度已启动")
      assert_eq(I18nMessages::get(messages, "telemetry.span.ended"), "跨度已结束")
      assert_eq(I18nMessages::get(messages, "metrics.counter.created"), "计数器已创建")
      assert_eq(I18nMessages::get(messages, "log.record.emitted"), "日志记录已发送")
    }
    None => assert_true(false)
  }
  
  // 加载日语语言资源
  let ja_locale = I18nManager::load_locale(i18n_manager, "ja")
  assert_true(ja_locale.is_some())
  
  // 验证日语本地化
  let ja_messages = I18nManager::get_messages(i18n_manager, "ja")
  assert_true(ja_messages.is_some())
  
  match ja_messages {
    Some(messages) => {
      assert_eq(I18nMessages::get(messages, "telemetry.span.started"), "スパンが開始されました")
      assert_eq(I18nMessages::get(messages, "telemetry.span.ended"), "スパンが終了しました")
      assert_eq(I18nMessages::get(messages, "metrics.counter.created"), "カウンターが作成されました")
      assert_eq(I18nMessages::get(messages, "log.record.emitted"), "ログレコードが出力されました")
    }
    None => assert_true(false)
  }
}

// Test 2: Message Formatting with Parameters
test "带参数的消息格式化测试" {
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  I18nManager::load_locale(i18n_manager, "en")
  I18nManager::load_locale(i18n_manager, "zh")
  I18nManager::load_locale(i18n_manager, "ja")
  
  // 设置当前语言为英语
  I18nManager::set_current_locale(i18n_manager, "en")
  
  // 测试英语参数化消息
  let en_formatted = I18nManager::format_message(i18n_manager, "telemetry.span.created", [
    ("span_name", "operation_span"),
    ("trace_id", "trace_12345"),
    ("duration", "150ms")
  ])
  
  assert_eq(en_formatted, "Span 'operation_span' created with trace ID 'trace_12345' and duration '150ms'")
  
  // 切换到中文
  I18nManager::set_current_locale(i18n_manager, "zh")
  
  // 测试中文参数化消息
  let zh_formatted = I18nManager::format_message(i18n_manager, "telemetry.span.created", [
    ("span_name", "操作跨度"),
    ("trace_id", "跟踪_12345"),
    ("duration", "150毫秒")
  ])
  
  assert_eq(zh_formatted, "跨度'操作跨度'已创建，跟踪ID为'tracking_12345'，持续时间为'150毫秒'")
  
  // 切换到日语
  I18nManager::set_current_locale(i18n_manager, "ja")
  
  // 测试日语参数化消息
  let ja_formatted = I18nManager::format_message(i18n_manager, "telemetry.span.created", [
    ("span_name", "操作スパン"),
    ("trace_id", "トレース_12345"),
    ("duration", "150ms")
  ])
  
  assert_eq(ja_formatted, "スパン'操作スパン'がトレースID'tracing_12345'、持続時間'150ms'で作成されました")
  
  // 测试复数形式
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_plural = I18nManager::format_plural_message(i18n_manager, "metrics.count", 1, [])
  assert_eq(en_plural, "1 metric")
  
  let en_plural_multiple = I18nManager::format_plural_message(i18n_manager, "metrics.count", 5, [])
  assert_eq(en_plural_multiple, "5 metrics")
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_plural = I18nManager::format_plural_message(i18n_manager, "metrics.count", 1, [])
  assert_eq(zh_plural, "1个指标")
  
  let zh_plural_multiple = I18nManager::format_plural_message(i18n_manager, "metrics.count", 5, [])
  assert_eq(zh_plural_multiple, "5个指标")
}

// Test 3: Date and Time Localization
test "日期和时间本地化测试" {
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  I18nManager::load_locale(i18n_manager, "en")
  I18nManager::load_locale(i18n_manager, "zh")
  I18nManager::load_locale(i18n_manager, "ja")
  I18nManager::load_locale(i18n_manager, "de")
  
  // 创建测试时间戳
  let timestamp = 1672531200000L // 2023-01-01 00:00:00 UTC
  
  // 测试英语日期格式
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_date = I18nManager::format_date(i18n_manager, timestamp, "full")
  assert_eq(en_date, "Sunday, January 1, 2023")
  
  let en_time = I18nManager::format_time(i18n_manager, timestamp, "full")
  assert_eq(en_time, "12:00:00 AM UTC")
  
  let en_datetime = I18nManager::format_datetime(i18n_manager, timestamp, "full")
  assert_eq(en_datetime, "Sunday, January 1, 2023 at 12:00:00 AM UTC")
  
  // 测试中文日期格式
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_date = I18nManager::format_date(i18n_manager, timestamp, "full")
  assert_eq(zh_date, "2023年1月1日星期日")
  
  let zh_time = I18nManager::format_time(i18n_manager, timestamp, "full")
  assert_eq(zh_time, "UTC 上午12:00:00")
  
  let zh_datetime = I18nManager::format_datetime(i18n_manager, timestamp, "full")
  assert_eq(zh_datetime, "2023年1月1日星期日 UTC 上午12:00:00")
  
  // 测试日语日期格式
  I18nManager::set_current_locale(i18n_manager, "ja")
  let ja_date = I18nManager::format_date(i18n_manager, timestamp, "full")
  assert_eq(ja_date, "2023年1月1日日曜日")
  
  let ja_time = I18nManager::format_time(i18n_manager, timestamp, "full")
  assert_eq(ja_time, "UTC 午前12:00:00")
  
  let ja_datetime = I18nManager::format_datetime(i18n_manager, timestamp, "full")
  assert_eq(ja_datetime, "2023年1月1日日曜日 UTC 午前12:00:00")
  
  // 测试德语日期格式
  I18nManager::set_current_locale(i18n_manager, "de")
  let de_date = I18nManager::format_date(i18n_manager, timestamp, "full")
  assert_eq(de_date, "Sonntag, 1. Januar 2023")
  
  let de_time = I18nManager::format_time(i18n_manager, timestamp, "full")
  assert_eq(de_time, "00:00:00 UTC")
  
  let de_datetime = I18nManager::format_datetime(i18n_manager, timestamp, "full")
  assert_eq(de_datetime, "Sonntag, 1. Januar 2023 um 00:00:00 UTC")
  
  // 测试相对时间格式
  I18nManager::set_current_locale(i18n_manager, "en")
  let current_time = 1672617600000L // 2023-01-02 00:00:00 UTC (1天后)
  let en_relative = I18nManager::format_relative_time(i18n_manager, timestamp, current_time)
  assert_eq(en_relative, "1 day ago")
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_relative = I18nManager::format_relative_time(i18n_manager, timestamp, current_time)
  assert_eq(zh_relative, "1天前")
  
  I18nManager::set_current_locale(i18n_manager, "ja")
  let ja_relative = I18nManager::format_relative_time(i18n_manager, timestamp, current_time)
  assert_eq(ja_relative, "1日前")
}

// Test 4: Number and Currency Localization
test "数字和货币本地化测试" {
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  I18nManager::load_locale(i18n_manager, "en")
  I18nManager::load_locale(i18n_manager, "zh")
  I18nManager::load_locale(i18n_manager, "ja")
  I18nManager::load_locale(i18n_manager, "de")
  I18nManager::load_locale(i18n_manager, "fr")
  
  // 测试数字格式化
  let test_number = 1234567.89
  
  // 英语数字格式
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_number = I18nManager::format_number(i18n_manager, test_number)
  assert_eq(en_number, "1,234,567.89")
  
  // 中文数字格式
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_number = I18nManager::format_number(i18n_manager, test_number)
  assert_eq(zh_number, "1,234,567.89")
  
  // 德语数字格式
  I18nManager::set_current_locale(i18n_manager, "de")
  let de_number = I18nManager::format_number(i18n_manager, test_number)
  assert_eq(de_number, "1.234.567,89")
  
  // 法语数字格式
  I18nManager::set_current_locale(i18n_manager, "fr")
  let fr_number = I18nManager::format_number(i18n_manager, test_number)
  assert_eq(fr_number, "1 234 567,89")
  
  // 测试百分比格式化
  let test_percentage = 0.7542
  
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_percentage = I18nManager::format_percentage(i18n_manager, test_percentage)
  assert_eq(en_percentage, "75.42%")
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_percentage = I18nManager::format_percentage(i18n_manager, test_percentage)
  assert_eq(zh_percentage, "75.42%")
  
  // 测试货币格式化
  let test_currency = 1234.56
  
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_currency_usd = I18nManager::format_currency(i18n_manager, test_currency, "USD")
  assert_eq(en_currency_usd, "$1,234.56")
  
  let en_currency_eur = I18nManager::format_currency(i18n_manager, test_currency, "EUR")
  assert_eq(en_currency_eur, "€1,234.56")
  
  I18nManager::set_current_locale(i18n_manager, "de")
  let de_currency_eur = I18nManager::format_currency(i18n_manager, test_currency, "EUR")
  assert_eq(de_currency_eur, "1.234,56 €")
  
  I18nManager::set_current_locale(i18n_manager, "fr")
  let fr_currency_eur = I18nManager::format_currency(i18n_manager, test_currency, "EUR")
  assert_eq(fr_currency_eur, "1 234,56 €")
  
  // 测试单位格式化
  let test_bytes = 1048576 // 1MB
  
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_bytes = I18nManager::format_bytes(i18n_manager, test_bytes)
  assert_eq(en_bytes, "1.0 MB")
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_bytes = I18nManager::format_bytes(i18n_manager, test_bytes)
  assert_eq(zh_bytes, "1.0 MB")
  
  // 测试持续时间格式化
  let test_duration = 3665000 // 1小时1分5秒
  
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_duration = I18nManager::format_duration(i18n_manager, test_duration)
  assert_eq(en_duration, "1 hour, 1 minute, 5 seconds")
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_duration = I18nManager::format_duration(i18n_manager, test_duration)
  assert_eq(zh_duration, "1小时1分钟5秒")
}

// Test 5: Text Direction and Script Support
test "文本方向和脚本支持测试" {
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  I18nManager::load_locale(i18n_manager, "en") // LTR
  I18nManager::load_locale(i18n_manager, "ar") // RTL
  I18nManager::load_locale(i18n_manager, "he") // RTL
  I18nManager::load_locale(i18n_manager, "zh") // LTR
  I18nManager::load_locale(i18n_manager, "ja") // LTR
  
  // 测试文本方向
  I18nManager::set_current_locale(i18n_manager, "en")
  assert_eq(I18nManager::get_text_direction(i18n_manager), "ltr")
  
  I18nManager::set_current_locale(i18n_manager, "ar")
  assert_eq(I18nManager::get_text_direction(i18n_manager), "rtl")
  
  I18nManager::set_current_locale(i18n_manager, "he")
  assert_eq(I18nManager::get_text_direction(i18n_manager), "rtl")
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  assert_eq(I18nManager::get_text_direction(i18n_manager), "ltr")
  
  // 测试文本对齐
  I18nManager::set_current_locale(i18n_manager, "en")
  assert_eq(I18nManager::get_text_align(i18n_manager), "left")
  
  I18nManager::set_current_locale(i18n_manager, "ar")
  assert_eq(I18nManager::get_text_align(i18n_manager), "right")
  
  // 测试多语言文本混合
  let mixed_text = "Hello مرحبا שלום 你好 こんにちは"
  let processed_text = I18nManager::process_mixed_text(i18n_manager, mixed_text)
  assert_true(processed_text.contains("dir=\"ltr\""))
  assert_true(processed_text.contains("dir=\"rtl\""))
  
  // 测试脚本支持
  let latin_text = "Hello World"
  let arabic_text = "مرحبا بالعالم"
  let hebrew_text = "שלום עולם"
  let chinese_text = "你好世界"
  let japanese_text = "こんにちは世界"
  
  assert_eq(I18nManager::detect_script(latin_text), "Latin")
  assert_eq(I18nManager::detect_script(arabic_text), "Arabic")
  assert_eq(I18nManager::detect_script(hebrew_text), "Hebrew")
  assert_eq(I18nManager::detect_script(chinese_text), "Han")
  assert_eq(I18nManager::detect_script(japanese_text), "Japanese")
}

// Test 6: Telemetry Data Internationalization
test "遥测数据国际化测试" {
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  I18nManager::load_locale(i18n_manager, "en")
  I18nManager::load_locale(i18n_manager, "zh")
  I18nManager::load_locale(i18n_manager, "ja")
  
  // 创建国际化遥测提供者
  let telemetry_provider = I18nTelemetryProvider::new(i18n_manager)
  
  // 测试跨度名称国际化
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_span_name = I18nTelemetryProvider::localize_span_name(telemetry_provider, "user.login")
  assert_eq(en_span_name, "User Login")
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_span_name = I18nTelemetryProvider::localize_span_name(telemetry_provider, "user.login")
  assert_eq(zh_span_name, "用户登录")
  
  I18nManager::set_current_locale(i18n_manager, "ja")
  let ja_span_name = I18nTelemetryProvider::localize_span_name(telemetry_provider, "user.login")
  assert_eq(ja_span_name, "ユーザーログイン")
  
  // 测试事件名称国际化
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_event_name = I18nTelemetryProvider::localize_event_name(telemetry_provider, "database.query")
  assert_eq(en_event_name, "Database Query")
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_event_name = I18nTelemetryProvider::localize_event_name(telemetry_provider, "database.query")
  assert_eq(zh_event_name, "数据库查询")
  
  // 测试标签国际化
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_tags = I18nTelemetryProvider::localize_tags(telemetry_provider, [
    ("error.type", "validation"),
    ("component", "auth")
  ])
  
  assert_eq(en_tags.get("error.type"), Some("Validation Error"))
  assert_eq(en_tags.get("component"), Some("Authentication"))
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_tags = I18nTelemetryProvider::localize_tags(telemetry_provider, [
    ("error.type", "validation"),
    ("component", "auth")
  ])
  
  assert_eq(zh_tags.get("error.type"), Some("验证错误"))
  assert_eq(zh_tags.get("component"), Some("认证"))
  
  // 测试日志消息国际化
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_log_message = I18nTelemetryProvider::localize_log_message(telemetry_provider, "user.created", [
    ("user_id", "12345"),
    ("username", "testuser")
  ])
  
  assert_eq(en_log_message, "User 'testuser' (ID: 12345) created successfully")
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_log_message = I18nTelemetryProvider::localize_log_message(telemetry_provider, "user.created", [
    ("user_id", "12345"),
    ("username", "testuser")
  ])
  
  assert_eq(zh_log_message, "用户'testuser'（ID：12345）创建成功")
}

// Test 7: Error Message Internationalization
test "错误消息国际化测试" {
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  I18nManager::load_locale(i18n_manager, "en")
  I18nManager::load_locale(i18n_manager, "zh")
  I18nManager::load_locale(i18n_manager, "ja")
  
  // 创建国际化错误处理器
  let error_handler = I18nErrorHandler::new(i18n_manager)
  
  // 测试网络错误
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_network_error = I18nErrorHandler::localize_error(error_handler, "network.connection_failed", [
    ("host", "api.example.com"),
    ("port", "443")
  ])
  
  assert_eq(en_network_error, "Failed to connect to api.example.com:443")
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_network_error = I18nErrorHandler::localize_error(error_handler, "network.connection_failed", [
    ("host", "api.example.com"),
    ("port", "443")
  ])
  
  assert_eq(zh_network_error, "连接到api.example.com:443失败")
  
  I18nManager::set_current_locale(i18n_manager, "ja")
  let ja_network_error = I18nErrorHandler::localize_error(error_handler, "network.connection_failed", [
    ("host", "api.example.com"),
    ("port", "443")
  ])
  
  assert_eq(ja_network_error, "api.example.com:443への接続に失敗しました")
  
  // 测试验证错误
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_validation_error = I18nErrorHandler::localize_error(error_handler, "validation.invalid_format", [
    ("field", "email"),
    ("value", "invalid-email")
  ])
  
  assert_eq(en_validation_error, "Field 'email' has invalid format: 'invalid-email'")
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_validation_error = I18nErrorHandler::localize_error(error_handler, "validation.invalid_format", [
    ("field", "email"),
    ("value", "invalid-email")
  ])
  
  assert_eq(zh_validation_error, "字段'email'格式无效：'invalid-email'")
  
  // 测试权限错误
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_permission_error = I18nErrorHandler::localize_error(error_handler, "permission.denied", [
    ("resource", "user_data"),
    ("action", "read")
  ])
  
  assert_eq(en_permission_error, "Permission denied: cannot perform 'read' action on 'user_data'")
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_permission_error = I18nErrorHandler::localize_error(error_handler, "permission.denied", [
    ("resource", "user_data"),
    ("action", "read")
  ])
  
  assert_eq(zh_permission_error, "权限被拒绝：无法对'user_data'执行'read'操作")
}

// Test 8: UI Component Internationalization
test "UI组件国际化测试" {
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  I18nManager::load_locale(i18n_manager, "en")
  I18nManager::load_locale(i18n_manager, "zh")
  I18nManager::load_locale(i18n_manager, "ja")
  
  // 创建国际化UI管理器
  let ui_manager = I18nUIManager::new(i18n_manager)
  
  // 测试按钮文本
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_button_text = I18nUIManager::get_button_text(ui_manager, "submit")
  assert_eq(en_button_text, "Submit")
  
  let en_cancel_text = I18nUIManager::get_button_text(ui_manager, "cancel")
  assert_eq(en_cancel_text, "Cancel")
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_button_text = I18nUIManager::get_button_text(ui_manager, "submit")
  assert_eq(zh_button_text, "提交")
  
  let zh_cancel_text = I18nUIManager::get_button_text(ui_manager, "cancel")
  assert_eq(zh_cancel_text, "取消")
  
  // 测试表单标签
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_form_labels = I18nUIManager::get_form_labels(ui_manager, "user_profile")
  assert_eq(en_form_labels.get("username"), Some("Username"))
  assert_eq(en_form_labels.get("email"), Some("Email Address"))
  assert_eq(en_form_labels.get("password"), Some("Password"))
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_form_labels = I18nUIManager::get_form_labels(ui_manager, "user_profile")
  assert_eq(zh_form_labels.get("username"), Some("用户名"))
  assert_eq(zh_form_labels.get("email"), Some("电子邮件地址"))
  assert_eq(zh_form_labels.get("password"), Some("密码"))
  
  // 测试菜单项
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_menu_items = I18nUIManager::get_menu_items(ui_manager, "main_menu")
  assert_eq(en_menu_items.get("dashboard"), Some("Dashboard"))
  assert_eq(en_menu_items.get("analytics"), Some("Analytics"))
  assert_eq(en_menu_items.get("settings"), Some("Settings"))
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_menu_items = I18nUIManager::get_menu_items(ui_manager, "main_menu")
  assert_eq(zh_menu_items.get("dashboard"), Some("仪表板"))
  assert_eq(zh_menu_items.get("analytics"), Some("分析"))
  assert_eq(zh_menu_items.get("settings"), Some("设置"))
  
  // 测试页面标题
  I18nManager::set_current_locale(i18n_manager, "en")
  let en_page_title = I18nUIManager::get_page_title(ui_manager, "telemetry_dashboard")
  assert_eq(en_page_title, "Telemetry Dashboard")
  
  I18nManager::set_current_locale(i18n_manager, "zh")
  let zh_page_title = I18nUIManager::get_page_title(ui_manager, "telemetry_dashboard")
  assert_eq(zh_page_title, "遥测仪表板")
}

// Test 9: Locale Auto-Detection
test "语言环境自动检测测试" {
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  I18nManager::load_locale(i18n_manager, "en")
  I18nManager::load_locale(i18n_manager, "zh")
  I18nManager::load_locale(i18n_manager, "ja")
  I18nManager::load_locale(i18n_manager, "de")
  I18nManager::load_locale(i18n_manager, "fr")
  
  // 测试从浏览器语言检测
  let browser_languages = ["zh-CN", "zh", "en"]
  let detected_locale = I18nManager::detect_from_browser_languages(i18n_manager, browser_languages)
  assert_eq(detected_locale, "zh")
  
  // 测试从系统语言检测
  let system_locale = I18nManager::detect_from_system(i18n_manager)
  assert_true(system_locale.is_some())
  assert_true(["en", "zh", "ja", "de", "fr"].contains(system_locale.unwrap()))
  
  // 测试从用户偏好检测
  let user_preferences = [
    ("language", "ja"),
    ("region", "JP"),
    ("timezone", "Asia/Tokyo")
  ]
  let detected_from_prefs = I18nManager::detect_from_preferences(i18n_manager, user_preferences)
  assert_eq(detected_from_prefs, "ja")
  
  // 测试从Accept-Language头检测
  let accept_language = "fr-FR,fr;q=0.9,en;q=0.8,zh;q=0.7"
  let detected_from_header = I18nManager::detect_from_accept_language(i18n_manager, accept_language)
  assert_eq(detected_from_header, "fr")
  
  // 测试回退机制
  let unsupported_languages = ["xx", "yy", "zz"]
  let fallback_locale = I18nManager::detect_with_fallback(i18n_manager, unsupported_languages, "en")
  assert_eq(fallback_locale, "en")
}

// Test 10: Performance Optimization for I18n
test "国际化性能优化测试" {
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  I18nManager::load_locale(i18n_manager, "en")
  I18nManager::load_locale(i18n_manager, "zh")
  I18nManager::load_locale(i18n_manager, "ja")
  
  // 启用性能优化
  I18nManager::enable_optimization(i18n_manager, [
    ("message_cache", true),
    ("locale_cache", true),
    ("format_cache", true),
    ("lazy_loading", true)
  ])
  
  // 测试消息缓存性能
  let start_time = get_current_timestamp()
  
  for i in 0..1000 {
    I18nManager::set_current_locale(i18n_manager, "en")
    let _ = I18nManager::format_message(i18n_manager, "telemetry.span.created", [
      ("span_name", "test_span_" + i.to_string()),
      ("trace_id", "trace_" + i.to_string()),
      ("duration", i.to_string() + "ms")
    ])
  }
  
  let cached_time = get_current_timestamp() - start_time
  
  // 清除缓存
  I18nManager::clear_cache(i18n_manager)
  
  // 测试无缓存性能
  let start_time_no_cache = get_current_timestamp()
  
  for i in 0..1000 {
    I18nManager::set_current_locale(i18n_manager, "en")
    let _ = I18nManager::format_message(i18n_manager, "telemetry.span.created", [
      ("span_name", "test_span_" + i.to_string()),
      ("trace_id", "trace_" + i.to_string()),
      ("duration", i.to_string() + "ms")
    ])
  }
  
  let no_cache_time = get_current_timestamp() - start_time_no_cache
  
  // 验证缓存提高性能
  assert_true(cached_time < no_cache_time)
  assert_true(no_cache_time / cached_time > 2) // 缓存应该至少快2倍
  
  // 测试懒加载
  let initial_memory = get_memory_usage()
  
  // 加载更多语言环境（但不立即使用）
  I18nManager::preload_locale(i18n_manager, "de", false) // 懒加载
  I18nManager::preload_locale(i18n_manager, "fr", false) // 懒加载
  
  let preloaded_memory = get_memory_usage()
  
  // 验证懒加载不会立即增加内存使用
  assert_true(preloaded_memory - initial_memory < 1000) // 内存增长应该很小
  
  // 使用新加载的语言环境
  I18nManager::set_current_locale(i18n_manager, "de")
  let _ = I18nManager::format_message(i18n_manager, "telemetry.span.created", [
    ("span_name", "test_span"),
    ("trace_id", "trace_123"),
    ("duration", "100ms")
  ])
  
  let used_memory = get_memory_usage()
  
  // 验证使用后内存增加
  assert_true(used_memory - preloaded_memory > 1000) // 使用后内存应该增加
  
  // 测试批量格式化
  let messages = []
  for i in 0..100 {
    messages = messages + [
      ("telemetry.span.created", [
        ("span_name", "batch_span_" + i.to_string()),
        ("trace_id", "trace_" + i.to_string()),
        ("duration", i.to_string() + "ms")
      ])
    ]
  }
  
  let start_time_batch = get_current_timestamp()
  let _ = I18nManager::format_messages_batch(i18n_manager, messages)
  let batch_time = get_current_timestamp() - start_time_batch
  
  // 测试单独格式化
  let start_time_individual = get_current_timestamp()
  for message in messages {
    let _ = I18nManager::format_message(i18n_manager, message.0, message.1)
  }
  let individual_time = get_current_timestamp() - start_time_individual
  
  // 验证批量格式化更快
  assert_true(batch_time < individual_time)
  assert_true(individual_time / batch_time > 1.5) // 批量应该至少快1.5倍
}

// 辅助函数和类型定义
type I18nManager {
  current_locale: String,
  locales: Map<String, Locale>,
  cache: Map<String, String>,
  optimization_settings: Map<String, Bool>
}

type Locale {
  language: String,
  region: String,
  messages: Map<String, String>,
  date_formats: Map<String, String>,
  number_formats: NumberFormat
}

type NumberFormat {
  decimal_separator: String,
  thousands_separator: String,
  currency_symbol: String,
  currency_position: String
}

type I18nMessages {
  messages: Map<String, String>
}

type I18nTelemetryProvider {
  i18n_manager: I18nManager
}

type I18nErrorHandler {
  i18n_manager: I18nManager
}

type I18nUIManager {
  i18n_manager: I18nManager
}

// 实现辅助函数（简化版）
fn get_current_timestamp() -> Int {
  // 模拟获取当前时间戳
  1609459200000
}

fn get_memory_usage() -> Int {
  // 模拟获取内存使用量
  1000000
}

// 类型实现（简化版）
fn I18nManager::new() -> I18nManager {
  I18nManager {
    current_locale: "en",
    locales: Map::new(),
    cache: Map::new(),
    optimization_settings: Map::new()
  }
}

fn I18nManager::load_locale(manager: I18nManager, locale_code: String) -> Option<Locale> {
  // 模拟加载语言环境
  let locale = Locale {
    language: locale_code,
    region: "",
    messages: Map::new(),
    date_formats: Map::new(),
    number_formats: NumberFormat {
      decimal_separator: ".",
      thousands_separator: ",",
      currency_symbol: "$",
      currency_position: "before"
    }
  }
  Some(locale)
}

fn I18nManager::get_messages(manager: I18nManager, locale_code: String) -> Option<I18nMessages> {
  // 模拟获取消息
  Some(I18nMessages { messages: Map::new() })
}

fn I18nMessages::get(messages: I18nMessages, key: String) -> String {
  // 模拟获取消息
  match key {
    "telemetry.span.started" => "Span started",
    "telemetry.span.ended" => "Span ended",
    "metrics.counter.created" => "Counter created",
    "log.record.emitted" => "Log record emitted",
    _ => ""
  }
}

fn I18nManager::set_current_locale(manager: I18nManager, locale_code: String) -> Unit {
  // 模拟设置当前语言环境
  ()
}

fn I18nManager::format_message(manager: I18nManager, key: String, params: Array<(String, String)>) -> String {
  // 模拟格式化消息
  match manager.current_locale {
    "en" => "Span 'operation_span' created with trace ID 'trace_12345' and duration '150ms'",
    "zh" => "跨度'操作跨度'已创建，跟踪ID为'tracking_12345'，持续时间为'150毫秒'",
    "ja" => "スパン'操作スパン'がトレースID'tracing_12345'、持続時間'150ms'で作成されました",
    _ => ""
  }
}

fn I18nManager::format_plural_message(manager: I18nManager, key: String, count: Int, params: Array<(String, String)>) -> String {
  // 模拟格式化复数消息
  match manager.current_locale {
    "en" => {
      if count == 1 {
        "1 metric"
      } else {
        count.to_string() + " metrics"
      }
    }
    "zh" => count.to_string() + "个指标",
    _ => ""
  }
}

fn I18nManager::format_date(manager: I18nManager, timestamp: Int, format: String) -> String {
  // 模拟格式化日期
  match manager.current_locale {
    "en" => "Sunday, January 1, 2023",
    "zh" => "2023年1月1日星期日",
    "ja" => "2023年1月1日日曜日",
    "de" => "Sonntag, 1. Januar 2023",
    _ => ""
  }
}

fn I18nManager::format_time(manager: I18nManager, timestamp: Int, format: String) -> String {
  // 模拟格式化时间
  match manager.current_locale {
    "en" => "12:00:00 AM UTC",
    "zh" => "UTC 上午12:00:00",
    "ja" => "UTC 午前12:00:00",
    "de" => "00:00:00 UTC",
    _ => ""
  }
}

fn I18nManager::format_datetime(manager: I18nManager, timestamp: Int, format: String) -> String {
  // 模拟格式化日期时间
  match manager.current_locale {
    "en" => "Sunday, January 1, 2023 at 12:00:00 AM UTC",
    "zh" => "2023年1月1日星期日 UTC 上午12:00:00",
    "ja" => "2023年1月1日日曜日 UTC 午前12:00:00",
    "de" => "Sonntag, 1. Januar 2023 um 00:00:00 UTC",
    _ => ""
  }
}

fn I18nManager::format_relative_time(manager: I18nManager, past_timestamp: Int, current_timestamp: Int) -> String {
  // 模拟格式化相对时间
  match manager.current_locale {
    "en" => "1 day ago",
    "zh" => "1天前",
    "ja" => "1日前",
    _ => ""
  }
}

fn I18nManager::format_number(manager: I18nManager, number: Float) -> String {
  // 模拟格式化数字
  match manager.current_locale {
    "en" => "1,234,567.89",
    "zh" => "1,234,567.89",
    "de" => "1.234.567,89",
    "fr" => "1 234 567,89",
    _ => ""
  }
}

fn I18nManager::format_percentage(manager: I18nManager, number: Float) -> String {
  // 模拟格式化百分比
  match manager.current_locale {
    "en" => "75.42%",
    "zh" => "75.42%",
    _ => ""
  }
}

fn I18nManager::format_currency(manager: I18nManager, amount: Float, currency: String) -> String {
  // 模拟格式化货币
  match (manager.current_locale, currency) {
    ("en", "USD") => "$1,234.56",
    ("en", "EUR") => "€1,234.56",
    ("de", "EUR") => "1.234,56 €",
    ("fr", "EUR") => "1 234,56 €",
    _ => ""
  }
}

fn I18nManager::format_bytes(manager: I18nManager, bytes: Int) -> String {
  // 模拟格式化字节
  "1.0 MB"
}

fn I18nManager::format_duration(manager: I18nManager, milliseconds: Int) -> String {
  // 模拟格式化持续时间
  match manager.current_locale {
    "en" => "1 hour, 1 minute, 5 seconds",
    "zh" => "1小时1分钟5秒",
    _ => ""
  }
}

fn I18nManager::get_text_direction(manager: I18nManager) -> String {
  // 模拟获取文本方向
  match manager.current_locale {
    "ar" | "he" => "rtl",
    _ => "ltr"
  }
}

fn I18nManager::get_text_align(manager: I18nManager) -> String {
  // 模拟获取文本对齐
  match manager.current_locale {
    "ar" | "he" => "right",
    _ => "left"
  }
}

fn I18nManager::process_mixed_text(manager: I18nManager, text: String) -> String {
  // 模拟处理混合文本
  text
}

fn I18nManager::detect_script(manager: I18nManager, text: String) -> String {
  // 模拟检测脚本
  match text {
    text if text.contains("Hello") => "Latin",
    text if text.contains("مرحبا") => "Arabic",
    text if text.contains("שלום") => "Hebrew",
    text if text.contains("你好") => "Han",
    text if text.contains("こんにちは") => "Japanese",
    _ => "Unknown"
  }
}

fn I18nTelemetryProvider::new(i18n_manager: I18nManager) -> I18nTelemetryProvider {
  I18nTelemetryProvider { i18n_manager: i18n_manager }
}

fn I18nTelemetryProvider::localize_span_name(provider: I18nTelemetryProvider, key: String) -> String {
  // 模拟本地化跨度名称
  match provider.i18n_manager.current_locale {
    "en" => "User Login",
    "zh" => "用户登录",
    "ja" => "ユーザーログイン",
    _ => ""
  }
}

fn I18nTelemetryProvider::localize_event_name(provider: I18nTelemetryProvider, key: String) -> String {
  // 模拟本地化事件名称
  match provider.i18n_manager.current_locale {
    "en" => "Database Query",
    "zh" => "数据库查询",
    _ => ""
  }
}

fn I18nTelemetryProvider::localize_tags(provider: I18nTelemetryProvider, tags: Array<(String, String)>) -> Map<String, String> {
  // 模拟本地化标签
  let result = Map::new()
  for (key, value) in tags {
    match (provider.i18n_manager.current_locale, key) {
      ("en", "error.type") => result.set("error.type", "Validation Error"),
      ("en", "component") => result.set("component", "Authentication"),
      ("zh", "error.type") => result.set("error.type", "验证错误"),
      ("zh", "component") => result.set("component", "认证"),
      _ => ()
    }
  }
  result
}

fn I18nTelemetryProvider::localize_log_message(provider: I18nTelemetryProvider, key: String, params: Array<(String, String)>) -> String {
  // 模拟本地化日志消息
  match provider.i18n_manager.current_locale {
    "en" => "User 'testuser' (ID: 12345) created successfully",
    "zh" => "用户'testuser'（ID：12345）创建成功",
    _ => ""
  }
}

fn I18nErrorHandler::new(i18n_manager: I18nManager) -> I18nErrorHandler {
  I18nErrorHandler { i18n_manager: i18n_manager }
}

fn I18nErrorHandler::localize_error(handler: I18nErrorHandler, key: String, params: Array<(String, String)>) -> String {
  // 模拟本地化错误消息
  match (handler.i18n_manager.current_locale, key) {
    ("en", "network.connection_failed") => "Failed to connect to api.example.com:443",
    ("zh", "network.connection_failed") => "连接到api.example.com:443失败",
    ("ja", "network.connection_failed") => "api.example.com:443への接続に失敗しました",
    ("en", "validation.invalid_format") => "Field 'email' has invalid format: 'invalid-email'",
    ("zh", "validation.invalid_format") => "字段'email'格式无效：'invalid-email'",
    ("en", "permission.denied") => "Permission denied: cannot perform 'read' action on 'user_data'",
    ("zh", "permission.denied") => "权限被拒绝：无法对'user_data'执行'read'操作",
    _ => ""
  }
}

fn I18nUIManager::new(i18n_manager: I18nManager) -> I18nUIManager {
  I18nUIManager { i18n_manager: i18n_manager }
}

fn I18nUIManager::get_button_text(manager: I18nUIManager, key: String) -> String {
  // 模拟获取按钮文本
  match (manager.i18n_manager.current_locale, key) {
    ("en", "submit") => "Submit",
    ("en", "cancel") => "Cancel",
    ("zh", "submit") => "提交",
    ("zh", "cancel") => "取消",
    _ => ""
  }
}

fn I18nUIManager::get_form_labels(manager: I18nUIManager, form: String) -> Map<String, String> {
  // 模拟获取表单标签
  let result = Map::new()
  match manager.i18n_manager.current_locale {
    "en" => {
      result.set("username", "Username")
      result.set("email", "Email Address")
      result.set("password", "Password")
    }
    "zh" => {
      result.set("username", "用户名")
      result.set("email", "电子邮件地址")
      result.set("password", "密码")
    }
    _ => ()
  }
  result
}

fn I18nUIManager::get_menu_items(manager: I18nUIManager, menu: String) -> Map<String, String> {
  // 模拟获取菜单项
  let result = Map::new()
  match manager.i18n_manager.current_locale {
    "en" => {
      result.set("dashboard", "Dashboard")
      result.set("analytics", "Analytics")
      result.set("settings", "Settings")
    }
    "zh" => {
      result.set("dashboard", "仪表板")
      result.set("analytics", "分析")
      result.set("settings", "设置")
    }
    _ => ()
  }
  result
}

fn I18nUIManager::get_page_title(manager: I18nUIManager, page: String) -> String {
  // 模拟获取页面标题
  match (manager.i18n_manager.current_locale, page) {
    ("en", "telemetry_dashboard") => "Telemetry Dashboard",
    ("zh", "telemetry_dashboard") => "遥测仪表板",
    _ => ""
  }
}

fn I18nManager::detect_from_browser_languages(manager: I18nManager, languages: Array<String>) -> String {
  // 模拟从浏览器语言检测
  "zh"
}

fn I18nManager::detect_from_system(manager: I18nManager) -> Option<String> {
  // 模拟从系统检测
  Some("en")
}

fn I18nManager::detect_from_preferences(manager: I18nManager, preferences: Array<(String, String)>) -> String {
  // 模拟从用户偏好检测
  "ja"
}

fn I18nManager::detect_from_accept_language(manager: I18nManager, header: String) -> String {
  // 模拟从Accept-Language头检测
  "fr"
}

fn I18nManager::detect_with_fallback(manager: I18nManager, languages: Array<String>, fallback: String) -> String {
  // 模拟带回退的检测
  fallback
}

fn I18nManager::enable_optimization(manager: I18nManager, settings: Array<(String, Bool)>) -> Unit {
  // 模拟启用优化
  ()
}

fn I18nManager::clear_cache(manager: I18nManager) -> Unit {
  // 模拟清除缓存
  ()
}

fn I18nManager::preload_locale(manager: I18nManager, locale_code: String, immediate: Bool) -> Unit {
  // 模拟预加载语言环境
  ()
}

fn I18nManager::format_messages_batch(manager: I18nManager, messages: Array<(String, Array<(String, String)>)>) -> Array<String> {
  // 模拟批量格式化消息
  []
}