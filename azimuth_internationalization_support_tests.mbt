// 阿兹米克国际化支持测试用例
// 专注于遥测系统的多语言支持、本地化和全球适配功能

// 测试1: 基本多语言支持
test "基本多语言支持测试" {
  let i18n_service = I18nService::new()
  
  // 加载语言资源
  I18nService::load_language(i18n_service, "en", [
    ("telemetry.cpu.usage", "CPU Usage"),
    ("telemetry.memory.usage", "Memory Usage"),
    ("telemetry.disk.usage", "Disk Usage"),
    ("telemetry.network.throughput", "Network Throughput"),
    ("alert.high.cpu", "High CPU usage detected"),
    ("alert.low.memory", "Low memory warning")
  ])
  
  I18nService::load_language(i18n_service, "zh-CN", [
    ("telemetry.cpu.usage", "CPU使用率"),
    ("telemetry.memory.usage", "内存使用率"),
    ("telemetry.disk.usage", "磁盘使用率"),
    ("telemetry.network.throughput", "网络吞吐量"),
    ("alert.high.cpu", "检测到高CPU使用率"),
    ("alert.low.memory", "内存不足警告")
  ])
  
  I18nService::load_language(i18n_service, "ja", [
    ("telemetry.cpu.usage", "CPU使用率"),
    ("telemetry.memory.usage", "メモリ使用率"),
    ("telemetry.disk.usage", "ディスク使用率"),
    ("telemetry.network.throughput", "ネットワークスループット"),
    ("alert.high.cpu", "CPU使用率が高いことを検出"),
    ("alert.low.memory", "メモリ不足の警告")
  ])
  
  // 测试英文翻译
  I18nService::set_current_language(i18n_service, "en")
  assert_eq(I18nService::translate(i18n_service, "telemetry.cpu.usage"), "CPU Usage")
  assert_eq(I18nService::translate(i18n_service, "alert.high.cpu"), "High CPU usage detected")
  
  // 测试中文翻译
  I18nService::set_current_language(i18n_service, "zh-CN")
  assert_eq(I18nService::translate(i18n_service, "telemetry.cpu.usage"), "CPU使用率")
  assert_eq(I18nService::translate(i18n_service, "alert.high.cpu"), "检测到高CPU使用率")
  
  // 测试日文翻译
  I18nService::set_current_language(i18n_service, "ja")
  assert_eq(I18nService::translate(i18n_service, "telemetry.cpu.usage"), "CPU使用率")
  assert_eq(I18nService::translate(i18n_service, "alert.high.cpu"), "CPU使用率が高いことを検出")
  
  // 测试未找到翻译的情况
  assert_eq(I18nService::translate(i18n_service, "nonexistent.key"), "nonexistent.key")
}

// 测试2: 遥测数据本地化
test "遥测数据本地化测试" {
  let i18n_service = I18nService::new()
  let telemetry_service = LocalizedTelemetryService::new(i18n_service)
  
  // 加载语言资源
  I18nService::load_language(i18n_service, "en", [
    ("unit.percent", "%"),
    ("unit.megabyte", "MB"),
    ("unit.millisecond", "ms"),
    ("format.datetime", "{0} {1}"),
    ("month.january", "January"),
    ("month.february", "February")
  ])
  
  I18nService::load_language(i18n_service, "zh-CN", [
    ("unit.percent", "%"),
    ("unit.megabyte", "MB"),
    ("unit.millisecond", "毫秒"),
    ("format.datetime", "{0}年{1}月{2}日"),
    ("month.january", "1月"),
    ("month.february", "2月")
  ])
  
  // 创建遥测数据
  let telemetry_data = TelemetryData::new()
  TelemetryData::add_metric(telemetry_data, "cpu.usage", 75.5, "percent")
  TelemetryData::add_metric(telemetry_data, "memory.usage", 1024, "megabyte")
  TelemetryData::add_metric(telemetry_data, "response.time", 125.3, "millisecond")
  
  // 设置时间戳
  let timestamp = Timestamp::from_ymd(2023, 1, 15)
  TelemetryData::set_timestamp(telemetry_data, timestamp)
  
  // 本地化为英文
  let localized_en = LocalizedTelemetryService::localize(telemetry_service, telemetry_data, "en")
  let metrics_en = LocalizedTelemetryData::get_formatted_metrics(localized_en)
  
  assert_true(metrics_en.any(fn(metric) {
    metric.contains("75.5%")
  }))
  assert_true(metrics_en.any(fn(metric) {
    metric.contains("1024 MB")
  }))
  assert_true(metrics_en.any(fn(metric) {
    metric.contains("125.3 ms")
  }))
  
  // 本地化为中文
  let localized_zh = LocalizedTelemetryService::localize(telemetry_service, telemetry_data, "zh-CN")
  let metrics_zh = LocalizedTelemetryData::get_formatted_metrics(localized_zh)
  
  assert_true(metrics_zh.any(fn(metric) {
    metric.contains("75.5%")
  }))
  assert_true(metrics_zh.any(fn(metric) {
    metric.contains("1024 MB")
  }))
  assert_true(metrics_zh.any(fn(metric) {
    metric.contains("125.3 毫秒")
  }))
  
  // 测试日期时间本地化
  let date_en = LocalizedTelemetryData::get_formatted_timestamp(localized_en)
  let date_zh = LocalizedTelemetryData::get_formatted_timestamp(localized_zh)
  
  assert_true(date_en.contains("January"))
  assert_true(date_zh.contains("2023年1月15日"))
}

// 测试3: 数字和日期格式本地化
test "数字和日期格式本地化测试" {
  let locale_service = LocaleService::new()
  
  // 配置区域设置
  LocaleService::configure_locale(locale_service, "en-US", [
    ("decimal.separator", "."),
    ("thousands.separator", ","),
    ("date.format", "MM/DD/YYYY"),
    ("time.format", "hh:mm:ss A"),
    ("timezone", "UTC")
  ])
  
  LocaleService::configure_locale(locale_service, "zh-CN", [
    ("decimal.separator", "."),
    ("thousands.separator", ","),
    ("date.format", "YYYY年MM月DD日"),
    ("time.format", "HH时mm分ss秒"),
    ("timezone", "Asia/Shanghai")
  ])
  
  LocaleService::configure_locale(locale_service, "de-DE", [
    ("decimal.separator", ","),
    ("thousands.separator", "."),
    ("date.format", "DD.MM.YYYY"),
    ("time.format", "HH:mm:ss"),
    ("timezone", "Europe/Berlin")
  ])
  
  // 测试数字格式化
  let number = 1234567.89
  
  let formatted_en = LocaleService::format_number(locale_service, number, "en-US")
  assert_eq(formatted_en, "1,234,567.89")
  
  let formatted_zh = LocaleService::format_number(locale_service, number, "zh-CN")
  assert_eq(formatted_zh, "1,234,567.89")
  
  let formatted_de = LocaleService::format_number(locale_service, number, "de-DE")
  assert_eq(formatted_de, "1.234.567,89")
  
  // 测试日期格式化
  let date = Timestamp::from_ymd(2023, 1, 15)
  let time = Timestamp::from_hms(14, 30, 45)
  
  let date_en = LocaleService::format_date(locale_service, date, "en-US")
  assert_eq(date_en, "01/15/2023")
  
  let date_zh = LocaleService::format_date(locale_service, date, "zh-CN")
  assert_eq(date_zh, "2023年01月15日")
  
  let date_de = LocaleService::format_date(locale_service, date, "de-DE")
  assert_eq(date_de, "15.01.2023")
  
  // 测试时间格式化
  let time_en = LocaleService::format_time(locale_service, time, "en-US")
  assert_eq(time_en, "02:30:45 PM")
  
  let time_zh = LocaleService::format_time(locale_service, time, "zh-CN")
  assert_eq(time_zh, "14时30分45秒")
  
  let time_de = LocaleService::format_time(locale_service, time, "de-DE")
  assert_eq(time_de, "14:30:45")
}

// 测试4: 警报和错误消息国际化
test "警报和错误消息国际化测试" {
  let alert_service = LocalizedAlertService::new()
  
  // 加载警报消息资源
  alert_service.load_messages("en", [
    ("alert.cpu.high", "CPU usage is {0}%, exceeding the threshold of {1}%"),
    ("alert.memory.low", "Memory usage is {0}%, below the minimum of {1}%"),
    ("error.connection.failed", "Failed to connect to telemetry service: {0}"),
    ("error.data.invalid", "Invalid data format: {0}")
  ])
  
  alert_service.load_messages("zh-CN", [
    ("alert.cpu.high", "CPU使用率为{0}%，超过了{1}%的阈值"),
    ("alert.memory.low", "内存使用率为{0}%，低于{1}%的最小值"),
    ("error.connection.failed", "连接遥测服务失败：{0}"),
    ("error.data.invalid", "无效的数据格式：{0}")
  ])
  
  alert_service.load_messages("ja", [
    ("alert.cpu.high", "CPU使用率は{0}%で、{1}%のしきい値を超えています"),
    ("alert.memory.low", "メモリ使用率は{0}%で、{1}%の最小値を下回っています"),
    ("error.connection.failed", "テレメトリサービスへの接続に失敗しました：{0}"),
    ("error.data.invalid", "無効なデータ形式：{0}")
  ])
  
  // 创建警报
  let cpu_alert = Alert::new("cpu.high", [
    ("current", "85.5"),
    ("threshold", "80.0")
  ])
  
  let memory_alert = Alert::new("memory.low", [
    ("current", "15.2"),
    ("threshold", "20.0")
  ])
  
  // 本地化警报消息（英文）
  let cpu_alert_en = alert_service.localize_alert(cpu_alert, "en")
  let memory_alert_en = alert_service.localize_alert(memory_alert, "en")
  
  assert_eq(cpu_alert_en, "CPU usage is 85.5%, exceeding the threshold of 80.0%")
  assert_eq(memory_alert_en, "Memory usage is 15.2%, below the minimum of 20.0%")
  
  // 本地化警报消息（中文）
  let cpu_alert_zh = alert_service.localize_alert(cpu_alert, "zh-CN")
  let memory_alert_zh = alert_service.localize_alert(memory_alert, "zh-CN")
  
  assert_eq(cpu_alert_zh, "CPU使用率为85.5%，超过了80.0%的阈值")
  assert_eq(memory_alert_zh, "内存使用率为15.2%，低于20.0%的最小值")
  
  // 本地化警报消息（日文）
  let cpu_alert_ja = alert_service.localize_alert(cpu_alert, "ja")
  let memory_alert_ja = alert_service.localize_alert(memory_alert, "ja")
  
  assert_eq(cpu_alert_ja, "CPU使用率は85.5%で、80.0%のしきい値を超えています")
  assert_eq(memory_alert_ja, "メモリ使用率は15.2%で、20.0%の最小値を下回っています")
  
  // 创建错误消息
  let connection_error = Error::new("connection.failed", ["timeout"])
  let data_error = Error::new("data.invalid", ["missing.timestamp"])
  
  // 本地化错误消息
  let connection_error_en = alert_service.localize_error(connection_error, "en")
  let data_error_zh = alert_service.localize_error(data_error, "zh-CN")
  
  assert_eq(connection_error_en, "Failed to connect to telemetry service: timeout")
  assert_eq(data_error_zh, "无效的数据格式：missing.timestamp")
}

// 测试5: 文本方向和RTL支持
test "文本方向和RTL支持测试" {
  let text_direction_service = TextDirectionService::new()
  
  // 配置文本方向
  TextDirectionService::configure_direction(text_direction_service, "en", "LTR")
  TextDirectionService::configure_direction(text_direction_service, "zh-CN", "LTR")
  TextDirectionService::configure_direction(text_direction_service, "ar", "RTL")
  TextDirectionService::configure_direction(text_direction_service, "he", "RTL")
  
  // 测试文本方向检测
  assert_eq(TextDirectionService::get_direction(text_direction_service, "en"), "LTR")
  assert_eq(TextDirectionService::get_direction(text_direction_service, "zh-CN"), "LTR")
  assert_eq(TextDirectionService::get_direction(text_direction_service, "ar"), "RTL")
  assert_eq(TextDirectionService::get_direction(text_direction_service, "he"), "RTL")
  
  // 创建仪表板布局
  let dashboard = Dashboard::new()
  
  // 添加LTR语言的组件
  Dashboard::add_component(dashboard, "header", "Telemetry Dashboard", "en")
  Dashboard::add_component(dashboard, "sidebar", "Navigation Menu", "en")
  Dashboard::add_component(dashboard, "content", "Main Content Area", "en")
  
  // 应用LTR布局
  let ltr_layout = TextDirectionService::apply_layout(dashboard, "en")
  assert_eq(DashboardLayout::get_header_position(ltr_layout), "top-left")
  assert_eq(DashboardLayout::get_sidebar_position(ltr_layout), "left")
  assert_eq(DashboardLayout::get_content_direction(ltr_layout), "ltr")
  
  // 添加RTL语言的组件
  let rtl_dashboard = Dashboard::new()
  Dashboard::add_component(rtl_dashboard, "header", "لوحة القياس عن بعد", "ar")
  Dashboard::add_component(rtl_dashboard, "sidebar", "قائمة التنقل", "ar")
  Dashboard::add_component(rtl_dashboard, "content", "منطقة المحتوى الرئيسية", "ar")
  
  // 应用RTL布局
  let rtl_layout = TextDirectionService::apply_layout(rtl_dashboard, "ar")
  assert_eq(DashboardLayout::get_header_position(rtl_layout), "top-right")
  assert_eq(DashboardLayout::get_sidebar_position(rtl_layout), "right")
  assert_eq(DashboardLayout::get_content_direction(rtl_layout), "rtl")
  
  // 测试混合文本方向
  let mixed_dashboard = Dashboard::new()
  Dashboard::add_component(mixed_dashboard, "title", "Telemetry Dashboard لوحة القياس", "en")
  
  let mixed_layout = TextDirectionService::apply_mixed_layout(mixed_dashboard, "en")
  assert_eq(DashboardLayout::get_text_direction(mixed_layout, "title"), "auto")
}

// 测试6: 时区和时间处理
test "时区和时间处理测试" {
  let timezone_service = TimezoneService::new()
  
  // 创建不同时区的时间戳
  let utc_time = Timestamp::from_ymd_hms(2023, 1, 15, 14, 30, 45)
  
  // 转换为不同时区
  let us_pacific_time = TimezoneService::convert_timezone(utc_time, "UTC", "America/Los_Angeles")
  let china_time = TimezoneService::convert_timezone(utc_time, "UTC", "Asia/Shanghai")
  let germany_time = TimezoneService::convert_timezone(utc_time, "UTC", "Europe/Berlin")
  
  // 验证时区转换
  assert_eq(TimezoneService::format_timezone_time(us_pacific_time, "en-US"), "01/15/2023 06:30:45 AM")
  assert_eq(TimezoneService::format_timezone_time(china_time, "zh-CN"), "2023年01月15日 22时30分45秒")
  assert_eq(TimezoneService::format_timezone_time(germany_time, "de-DE"), "15.01.2023 15:30:45")
  
  // 测试夏令时处理
  let summer_time = Timestamp::from_ymd_hms(2023, 7, 15, 14, 30, 45)
  let us_pacific_summer = TimezoneService::convert_timezone(summer_time, "UTC", "America/Los_Angeles")
  
  assert_eq(TimezoneService::format_timezone_time(us_pacific_summer, "en-US"), "07/15/2023 07:30:45 AM")
  
  // 测试相对时间格式化
  let now = Timestamp::now()
  let one_hour_ago = now - (60 * 60 * 1000)
  let one_day_ago = now - (24 * 60 * 60 * 1000)
  let one_week_ago = now - (7 * 24 * 60 * 60 * 1000)
  
  let relative_en = TimezoneService::format_relative_time(one_hour_ago, now, "en")
  let relative_zh = TimezoneService::format_relative_time(one_hour_ago, now, "zh-CN")
  
  assert_eq(relative_en, "1 hour ago")
  assert_eq(relative_zh, "1小时前")
  
  let relative_day_en = TimezoneService::format_relative_time(one_day_ago, now, "en")
  let relative_day_zh = TimezoneService::format_relative_time(one_day_ago, now, "zh-CN")
  
  assert_eq(relative_day_en, "1 day ago")
  assert_eq(relative_day_zh, "1天前")
  
  // 测试工作日和月份名称本地化
  let weekday_en = TimezoneService::format_weekday(now, "en")
  let weekday_zh = TimezoneService::format_weekday(now, "zh-CN")
  
  assert_true(weekday_en.is_one_of(["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]))
  assert_true(weekday_zh.is_one_of(["星期一", "星期二", "星期三", "星期四", "星期五", "星期六", "星期日"]))
}

// 测试7: 多语言仪表板和UI组件
test "多语言仪表板和UI组件测试" {
  let ui_service = LocalizedUIService::new()
  
  // 加载UI文本资源
  ui_service.load_ui_text("en", [
    ("dashboard.title", "Telemetry Dashboard"),
    ("dashboard.cpu.chart", "CPU Usage"),
    ("dashboard.memory.chart", "Memory Usage"),
    ("button.refresh", "Refresh"),
    ("button.export", "Export"),
    ("menu.settings", "Settings"),
    ("menu.logout", "Logout")
  ])
  
  ui_service.load_ui_text("zh-CN", [
    ("dashboard.title", "遥测仪表板"),
    ("dashboard.cpu.chart", "CPU使用率"),
    ("dashboard.memory.chart", "内存使用率"),
    ("button.refresh", "刷新"),
    ("button.export", "导出"),
    ("menu.settings", "设置"),
    ("menu.logout", "退出登录")
  ])
  
  ui_service.load_ui_text("ja", [
    ("dashboard.title", "テレメトリダッシュボード"),
    ("dashboard.cpu.chart", "CPU使用率"),
    ("dashboard.memory.chart", "メモリ使用率"),
    ("button.refresh", "更新"),
    ("button.export", "エクスポート"),
    ("menu.settings", "設定"),
    ("menu.logout", "ログアウト")
  ])
  
  // 创建多语言仪表板
  let dashboard = LocalizedDashboard::new()
  
  // 添加UI组件
  Dashboard::add_title(dashboard, "dashboard.title")
  Dashboard::add_chart(dashboard, "cpu", "dashboard.cpu.chart")
  Dashboard::add_chart(dashboard, "memory", "dashboard.memory.chart")
  Dashboard::add_button(dashboard, "refresh", "button.refresh")
  Dashboard::add_button(dashboard, "export", "button.export")
  Dashboard::add_menu_item(dashboard, "settings", "menu.settings")
  Dashboard::add_menu_item(dashboard, "logout", "menu.logout")
  
  // 渲染英文仪表板
  let english_dashboard = ui_service.render_dashboard(dashboard, "en")
  assert_true(english_dashboard.contains("Telemetry Dashboard"))
  assert_true(english_dashboard.contains("CPU Usage"))
  assert_true(english_dashboard.contains("Memory Usage"))
  assert_true(english_dashboard.contains("Refresh"))
  assert_true(english_dashboard.contains("Export"))
  assert_true(english_dashboard.contains("Settings"))
  assert_true(english_dashboard.contains("Logout"))
  
  // 渲染中文仪表板
  let chinese_dashboard = ui_service.render_dashboard(dashboard, "zh-CN")
  assert_true(chinese_dashboard.contains("遥测仪表板"))
  assert_true(chinese_dashboard.contains("CPU使用率"))
  assert_true(chinese_dashboard.contains("内存使用率"))
  assert_true(chinese_dashboard.contains("刷新"))
  assert_true(chinese_dashboard.contains("导出"))
  assert_true(chinese_dashboard.contains("设置"))
  assert_true(chinese_dashboard.contains("退出登录"))
  
  // 渲染日文仪表板
  let japanese_dashboard = ui_service.render_dashboard(dashboard, "ja")
  assert_true(japanese_dashboard.contains("テレメトリダッシュボード"))
  assert_true(japanese_dashboard.contains("CPU使用率"))
  assert_true(japanese_dashboard.contains("メモリ使用率"))
  assert_true(japanese_dashboard.contains("更新"))
  assert_true(japanese_dashboard.contains("エクスポート"))
  assert_true(japanese_dashboard.contains("設定"))
  assert_true(japanese_dashboard.contains("ログアウト"))
  
  // 测试动态语言切换
  let language_switcher = LanguageSwitcher::new(["en", "zh-CN", "ja"])
  LanguageSwitcher::set_current_language(language_switcher, "en")
  
  let switched_dashboard = LanguageSwitcher::switch_language(language_switcher, dashboard, "zh-CN")
  assert_true(switched_dashboard.contains("遥测仪表板"))
  
  // 测试语言偏好设置
  let user_preferences = UserPreferences::new()
  UserPreferences::set_language(user_preferences, "ja")
  
  let personalized_dashboard = ui_service.render_dashboard_with_preferences(dashboard, user_preferences)
  assert_true(personalized_dashboard.contains("テレメトリダッシュボード"))
}

// 测试8: 文化适应性和本地化约定
test "文化适应性和本地化约定测试" {
  let culture_service = CultureService::new()
  
  // 配置文化约定
  CultureService::configure_culture(culture_service, "en-US", [
    ("number.grouping", "true"),
    ("currency.symbol", "$"),
    ("currency.position", "before"),
    ("date.first_day_of_week", "0"), // Sunday
    ("paper.size", "letter"),
    ("measurement.system", "imperial")
  ])
  
  CultureService::configure_culture(culture_service, "zh-CN", [
    ("number.grouping", "true"),
    ("currency.symbol", "¥"),
    ("currency.position", "before"),
    ("date.first_day_of_week", "1"), // Monday
    ("paper.size", "A4"),
    ("measurement.system", "metric")
  ])
  
  CultureService::configure_culture(culture_service, "fr-FR", [
    ("number.grouping", "true"),
    ("currency.symbol", "€"),
    ("currency.position", "after"),
    ("date.first_day_of_week", "1"), // Monday
    ("paper.size", "A4"),
    ("measurement.system", "metric")
  ])
  
  // 测试货币格式化
  let amount = 1234.56
  
  let currency_en = CultureService::format_currency(culture_service, amount, "en-US")
  assert_eq(currency_en, "$1,234.56")
  
  let currency_zh = CultureService::format_currency(culture_service, amount, "zh-CN")
  assert_eq(currency_zh, "¥1,234.56")
  
  let currency_fr = CultureService::format_currency(culture_service, amount, "fr-FR")
  assert_eq(currency_fr, "1,234.56€")
  
  // 测试度量单位系统
  let distance = 100.0
  
  let distance_imperial = CultureService::format_distance(culture_service, distance, "en-US")
  assert_eq(distance_imperial, "100.0 miles")
  
  let distance_metric = CultureService::format_distance(culture_service, distance, "zh-CN")
  assert_eq(distance_metric, "100.0 kilometers")
  
  // 测试文化特定的颜色和符号
  let color_en = CultureService::get_cultural_color(culture_service, "success", "en-US")
  let color_zh = CultureService::get_cultural_color(culture_service, "success", "zh-CN")
  
  // 在不同文化中，成功可能用不同颜色表示
  assert_eq(color_en, "#00FF00") // 绿色
  assert_eq(color_zh, "#FF0000") // 在中国文化中，红色也可能表示成功
  
  // 测试文化特定的排序规则
  let names = ["Zhang Wei", "Li Ming", "Wang Fang", "Chen Jing"]
  
  let sorted_en = CultureService::sort_strings(culture_service, names, "en-US")
  let sorted_zh = CultureService::sort_strings(culture_service, names, "zh-CN")
  
  // 英文排序（按字母顺序）
  assert_eq(sorted_en[0], "Chen Jing")
  assert_eq(sorted_en[3], "Zhang Wei")
  
  // 中文排序（按拼音顺序）
  assert_eq(sorted_zh[0], "Chen Jing") // 陈
  assert_eq(sorted_zh[1], "Li Ming")   // 李
  assert_eq(sorted_zh[2], "Wang Fang") // 王
  assert_eq(sorted_zh[3], "Zhang Wei") // 张
  
  // 测试文化敏感的文本处理
  let text = "Hello, 世界!"
  
  let processed_en = CultureService::process_text(culture_service, text, "en-US")
  let processed_zh = CultureService::process_text(culture_service, text, "zh-CN")
  
  // 不同文化可能有不同的文本处理规则
  assert_eq(processed_en, "Hello, 世界!") // 保持原样
  assert_eq(processed_zh, "你好，世界!")   // 可能进行文化适应的翻译
}