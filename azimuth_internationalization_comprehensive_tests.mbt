// Azimuth Internationalization and Globalization Test Suite
// 测试遥测系统的国际化和全球化支持

test "多语言错误消息和日志" {
  // 创建具有国际化支持的遥测提供者
  let telemetry_provider = TelemetryProvider::with_i18n_support()
  
  // 配置支持的语言
  let i18n_config = I18nConfig {
    default_locale: "en-US",
    supported_locales: ["en-US", "zh-CN", "ja-JP", "es-ES", "fr-FR", "de-DE"],
    fallback_strategy: "default_locale",
    resource_bundle_path: "/locales/telemetry"
  }
  
  I18nSupport::configure(telemetry_provider, i18n_config)
  
  // 加载资源包
  I18nSupport::load_resource_bundle(telemetry_provider, "en-US", {
    "error.connection.failed" => "Connection to telemetry backend failed",
    "error.invalid.metric" => "Invalid metric value: {0}",
    "info.operation.started" => "Operation '{0}' started successfully",
    "warning.rate.limit.exceeded" => "Rate limit exceeded: {0} requests per minute"
  })
  
  I18nSupport::load_resource_bundle(telemetry_provider, "zh-CN", {
    "error.connection.failed" => "遥测后端连接失败",
    "error.invalid.metric" => "无效的度量值: {0}",
    "info.operation.started" => "操作'{0}'成功启动",
    "warning.rate.limit.exceeded" => "速率限制超出: 每分钟{0}个请求"
  })
  
  I18nSupport::load_resource_bundle(telemetry_provider, "ja-JP", {
    "error.connection.failed" => "テレメトリバックエンドへの接続に失敗しました",
    "error.invalid.metric" => "無効なメトリック値: {0}",
    "info.operation.started" => "操作'{0}'が正常に開始されました",
    "warning.rate.limit.exceeded" => "レート制限を超過しました: 毎分{0}リクエスト"
  })
  
  // 创建tracer
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "i18n.test")
  
  // 测试英文错误消息
  I18nSupport::set_current_locale(telemetry_provider, "en-US")
  let en_span = Tracer::start_span(tracer, "i18n.test.en")
  Span::add_event(en_span, "error.occurred", [
    ("error.message", I18nSupport::get_message(telemetry_provider, "error.connection.failed")),
    ("locale", "en-US")
  ])
  Span::end(en_span)
  
  // 测试中文错误消息
  I18nSupport::set_current_locale(telemetry_provider, "zh-CN")
  let zh_span = Tracer::start_span(tracer, "i18n.test.zh")
  Span::add_event(zh_span, "error.occurred", [
    ("error.message", I18nSupport::get_message(telemetry_provider, "error.connection.failed")),
    ("locale", "zh-CN")
  ])
  Span::end(zh_span)
  
  // 测试日文错误消息
  I18nSupport::set_current_locale(telemetry_provider, "ja-JP")
  let ja_span = Tracer::start_span(tracer, "i18n.test.ja")
  Span::add_event(ja_span, "error.occurred", [
    ("error.message", I18nSupport::get_message(telemetry_provider, "error.connection.failed")),
    ("locale", "ja-JP")
  ])
  Span::end(ja_span)
  
  // 验证消息本地化
  let en_message = I18nSupport::get_message(telemetry_provider, "error.connection.failed", "en-US")
  assert_eq(en_message, "Connection to telemetry backend failed")
  
  let zh_message = I18nSupport::get_message(telemetry_provider, "error.connection.failed", "zh-CN")
  assert_eq(zh_message, "遥测后端连接失败")
  
  let ja_message = I18nSupport::get_message(telemetry_provider, "error.connection.failed", "ja-JP")
  assert_eq(ja_message, "テレメトリバックエンドへの接続に失敗しました")
  
  // 测试参数化消息
  let zh_param_message = I18nSupport::get_parameterized_message(
    telemetry_provider, 
    "error.invalid.metric", 
    "zh-CN", 
    ["-1.5"]
  )
  assert_eq(zh_param_message, "无效的度量值: -1.5")
  
  // 测试回退机制
  let unsupported_locale_message = I18nSupport::get_message(
    telemetry_provider, 
    "error.connection.failed", 
    "ko-KR" // 不支持的locale
  )
  assert_eq(unsupported_locale_message, "Connection to telemetry backend failed") // 回退到默认
  
  assert_true(true)
}

test "时区、日期和时间格式本地化" {
  // 创建具有时区支持的遥测提供者
  let telemetry_provider = TelemetryProvider::with_timezone_support()
  
  // 配置时区设置
  let timezone_config = TimezoneConfig {
    default_timezone: "UTC",
    supported_timezones: ["UTC", "America/New_York", "Asia/Shanghai", "Europe/London", "Asia/Tokyo"],
    auto_detect_timezone: true,
    preserve_original_timestamps: true
  }
  
  TimezoneSupport::configure(telemetry_provider, timezone_config)
  
  // 创建tracer
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "timezone.test")
  
  // 创建测试时间戳 (2025-01-02 10:00:00 UTC)
  let test_timestamp = Timestamp::from_string("2025-01-02T10:00:00Z")
  
  // 测试不同时区的时间转换
  let utc_time = TimezoneSupport::convert_timezone(telemetry_provider, test_timestamp, "UTC")
  assert_eq(Timestamp::to_string(utc_time), "2025-01-02T10:00:00Z")
  
  let ny_time = TimezoneSupport::convert_timezone(telemetry_provider, test_timestamp, "America/New_York")
  assert_eq(Timestamp::to_string(ny_time), "2025-01-02T05:00:00-05:00") // UTC-5 in January
  
  let shanghai_time = TimezoneSupport::convert_timezone(telemetry_provider, test_timestamp, "Asia/Shanghai")
  assert_eq(Timestamp::to_string(shanghai_time), "2025-01-02T18:00:00+08:00") // UTC+8
  
  let london_time = TimezoneSupport::convert_timezone(telemetry_provider, test_timestamp, "Europe/London")
  assert_eq(Timestamp::to_string(london_time), "2025-01-02T10:00:00+00:00") // UTC+0 in January
  
  let tokyo_time = TimezoneSupport::convert_timezone(telemetry_provider, test_timestamp, "Asia/Tokyo")
  assert_eq(Timestamp::to_string(tokyo_time), "2025-01-02T19:00:00+09:00") // UTC+9
  
  // 创建span并测试本地化时间戳
  let span = Tracer::start_span(tracer, "timezone.test.operation")
  
  // 添加不同时区的时间戳事件
  Span::add_event(span, "operation.started", [
    ("timestamp.utc", Timestamp::to_string(utc_time)),
    ("timestamp.ny", Timestamp::to_string(ny_time)),
    ("timestamp.shanghai", Timestamp::to_string(shanghai_time)),
    ("timestamp.london", Timestamp::to_string(london_time)),
    ("timestamp.tokyo", Timestamp::to_string(tokyo_time))
  ])
  
  Span::end(span)
  
  // 测试本地化日期格式
  let date_formats = [
    ("en-US", "01/02/2025"),
    ("zh-CN", "2025/01/02"),
    ("ja-JP", "2025/01/02"),
    ("de-DE", "02.01.2025"),
    ("fr-FR", "02/01/2025")
  ]
  
  for (locale, expected_format) in date_formats {
    let localized_date = TimezoneSupport::format_date(telemetry_provider, test_timestamp, locale)
    assert_eq(localized_date, expected_format)
  }
  
  // 测试本地化时间格式
  let time_formats = [
    ("en-US", "10:00:00 AM"),
    ("zh-CN", "上午10:00:00"),
    ("ja-JP", "10:00:00"),
    ("de-DE", "10:00:00"),
    ("fr-FR", "10:00:00")
  ]
  
  for (locale, expected_format) in time_formats {
    let localized_time = TimezoneSupport::format_time(telemetry_provider, test_timestamp, locale)
    assert_eq(localized_time, expected_format)
  }
  
  assert_true(true)
}

test "数字、货币和度量单位本地化" {
  // 创建具有数字本地化支持的遥测提供者
  let telemetry_provider = TelemetryProvider::with_number_localization()
  
  // 配置数字本地化设置
  let number_config = NumberLocalizationConfig {
    default_locale: "en-US",
    digit_grouping_symbol: ",",
    decimal_symbol: ".",
    currency_symbols: [
      ("en-US", "$"),
      ("zh-CN", "¥"),
      ("ja-JP", "¥"),
      ("de-DE", "€"),
      ("fr-FR", "€")
    ],
    measurement_units: "metric"
  }
  
  NumberLocalization::configure(telemetry_provider, number_config)
  
  // 创建tracer
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "number.localization.test")
  
  // 测试数字格式本地化
  let test_number = 1234567.89
  
  let en_number = NumberLocalization::format_number(telemetry_provider, test_number, "en-US")
  assert_eq(en_number, "1,234,567.89")
  
  let zh_number = NumberLocalization::format_number(telemetry_provider, test_number, "zh-CN")
  assert_eq(zh_number, "1,234,567.89") // 中文也使用逗号作为千位分隔符
  
  let de_number = NumberLocalization::format_number(telemetry_provider, test_number, "de-DE")
  assert_eq(de_number, "1.234.567,89") // 德语使用点作为千位分隔符，逗号作为小数点
  
  let fr_number = NumberLocalization::format_number(telemetry_provider, test_number, "fr-FR")
  assert_eq(fr_number, "1 234 567,89") // 法语使用空格作为千位分隔符
  
  // 测试货币格式本地化
  let test_currency = 1234.56
  
  let en_currency = NumberLocalization::format_currency(telemetry_provider, test_currency, "en-US", "USD")
  assert_eq(en_currency, "$1,234.56")
  
  let zh_currency = NumberLocalization::format_currency(telemetry_provider, test_currency, "zh-CN", "CNY")
  assert_eq(zh_currency, "¥1,234.56")
  
  let ja_currency = NumberLocalization::format_currency(telemetry_provider, test_currency, "ja-JP", "JPY")
  assert_eq(ja_currency, "¥1,234.56")
  
  let de_currency = NumberLocalization::format_currency(telemetry_provider, test_currency, "de-DE", "EUR")
  assert_eq(de_currency, "1.234,56 €")
  
  let fr_currency = NumberLocalization::format_currency(telemetry_provider, test_currency, "fr-FR", "EUR")
  assert_eq(fr_currency, "1 234,56 €")
  
  // 测试百分比格式本地化
  let test_percentage = 0.7542
  
  let en_percentage = NumberLocalization::format_percentage(telemetry_provider, test_percentage, "en-US")
  assert_eq(en_percentage, "75.42%")
  
  let zh_percentage = NumberLocalization::format_percentage(telemetry_provider, test_percentage, "zh-CN")
  assert_eq(zh_percentage, "75.42%")
  
  let de_percentage = NumberLocalization::format_percentage(telemetry_provider, test_percentage, "de-DE")
  assert_eq(de_percentage, "75,42 %")
  
  // 创建span并测试本地化数字
  let span = Tracer::start_span(tracer, "number.localization.operation")
  
  // 添加本地化数字属性
  Span::set_attribute(span, "revenue.usd", en_currency)
  Span::set_attribute(span, "revenue.cny", zh_currency)
  Span::set_attribute(span, "revenue.eur", de_currency)
  Span::set_attribute(span, "conversion.rate", en_percentage)
  Span::set_attribute(span, "user.count", en_number)
  
  Span::end(span)
  
  assert_true(true)
}

test "度量单位系统和转换" {
  // 创建具有度量单位支持的遥测提供者
  let telemetry_provider = TelemetryProvider::with_unit_system_support()
  
  // 配置度量单位系统
  let unit_config = UnitSystemConfig {
    default_system: "metric",
    supported_systems: ["metric", "imperial", "si"],
    auto_convert_units: true,
    precision: 4
  }
  
  UnitSystemSupport::configure(telemetry_provider, unit_config)
  
  // 创建tracer
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "unit.system.test")
  
  // 测试长度单位转换
  let metric_length = 100.0 // 100米
  let imperial_length = UnitSystemSupport::convert_length(telemetry_provider, metric_length, "metric", "imperial")
  assert_true(abs(imperial_length - 328.084) < 0.001) // 约328.084英尺
  
  // 测试重量单位转换
  let metric_weight = 70.0 // 70公斤
  let imperial_weight = UnitSystemSupport::convert_weight(telemetry_provider, metric_weight, "metric", "imperial")
  assert_true(abs(imperial_weight - 154.324) < 0.001) // 约154.324磅
  
  // 测试温度单位转换
  let celsius_temp = 25.0 // 25摄氏度
  let fahrenheit_temp = UnitSystemSupport::convert_temperature(telemetry_provider, celsius_temp, "celsius", "fahrenheit")
  assert_true(abs(fahrenheit_temp - 77.0) < 0.001) // 77华氏度
  
  // 测试速度单位转换
  let metric_speed = 100.0 // 100公里/小时
  let imperial_speed = UnitSystemSupport::convert_speed(telemetry_provider, metric_speed, "metric", "imperial")
  assert_true(abs(imperial_speed - 62.137) < 0.001) // 约62.137英里/小时
  
  // 创建span并测试单位转换
  let span = Tracer::start_span(tracer, "unit.conversion.operation")
  
  // 添加不同单位的度量值
  Span::set_attribute(span, "distance.metric", "100.0 km")
  Span::set_attribute(span, "distance.imperial", "62.137 mi")
  Span::set_attribute(span, "temperature.celsius", "25.0 °C")
  Span::set_attribute(span, "temperature.fahrenheit", "77.0 °F")
  Span::set_attribute(span, "weight.metric", "70.0 kg")
  Span::set_attribute(span, "weight.imperial", "154.324 lbs")
  
  Span::end(span)
  
  // 测试本地化单位格式
  let localized_distance = UnitSystemSupport::format_distance(telemetry_provider, 100.0, "metric", "en-US")
  assert_eq(localized_distance, "100 km")
  
  let localized_distance_imperial = UnitSystemSupport::format_distance(telemetry_provider, 62.137, "imperial", "en-US")
  assert_eq(localized_distance_imperial, "62.137 mi")
  
  let localized_temperature = UnitSystemSupport::format_temperature(telemetry_provider, 25.0, "celsius", "en-US")
  assert_eq(localized_temperature, "25°C")
  
  let localized_temperature_fahrenheit = UnitSystemSupport::format_temperature(telemetry_provider, 77.0, "fahrenheit", "en-US")
  assert_eq(localized_temperature_fahrenheit, "77°F")
  
  assert_true(true)
}