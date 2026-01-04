// Azimuth Internationalization Test Suite
// 国际化和本地化测试套件 - 专注于多语言支持、地区格式化和文化适配

// 测试1: 多语言资源管理
test "多语言资源管理测试" {
  // 创建国际化资源管理器
  let i18n_manager = I18nResourceManager::new()
  I18nResourceManager::set_default_locale(i18n_manager, "en-US")
  I18nResourceManager::set_fallback_locale(i18n_manager, "en-US")
  I18nResourceManager::set_resource_path(i18n_manager, "/resources/i18n")
  
  // 初始化资源管理器
  let init_result = I18nResourceManager::initialize(i18n_manager)
  assert_true(init_result.success)
  
  // 加载语言资源
  let supported_locales = ["en-US", "zh-CN", "ja-JP", "fr-FR", "de-DE", "es-ES", "pt-BR", "ru-RU"]
  
  for locale in supported_locales {
    let load_result = I18nResourceManager::load_locale(i18n_manager, locale)
    assert_true(load_result.success)
  }
  
  // 验证已加载的语言
  let loaded_locales = I18nResourceManager::get_loaded_locales(i18n_manager)
  assert_true(loaded_locales.length() >= supported_locales.length())
  
  for locale in supported_locales {
    assert_true(loaded_locales.contains(locale))
  }
  
  // 测试资源键值获取
  let test_keys = [
    "telemetry.span.name",
    "telemetry.span.status.ok",
    "telemetry.span.status.error",
    "telemetry.metric.cpu.usage",
    "telemetry.error.connection.timeout",
    "telemetry.service.azimuth.name",
    "telemetry.dashboard.title",
    "telemetry.config.sampling.rate"
  ]
  
  // 测试英文资源
  I18nResourceManager::set_current_locale(i18n_manager, "en-US")
  
  for key in test_keys {
    let get_result = I18nResourceManager::get_string(i18n_manager, key)
    assert_true(get_result.success)
    assert_true(get_result.value.length() > 0)
  }
  
  // 测试中文资源
  I18nResourceManager::set_current_locale(i18n_manager, "zh-CN")
  
  let zh_results = []
  for key in test_keys {
    let get_result = I18nResourceManager::get_string(i18n_manager, key)
    assert_true(get_result.success)
    zh_results.push(get_result.value)
  }
  
  // 验证中文资源包含中文字符
  for value in zh_results {
    assert_true(value.contains("遥测") || value.contains("跨度") || 
                 value.contains("状态") || value.contains("服务") ||
                 value.contains("配置") || value.contains("采样"))
  }
  
  // 测试日文资源
  I18nResourceManager::set_current_locale(i18n_manager, "ja-JP")
  
  for key in test_keys {
    let get_result = I18nResourceManager::get_string(i18n_manager, key)
    assert_true(get_result.success)
    assert_true(get_result.value.contains("テレメトリ") || 
                 get_result.value.contains("スパン") ||
                 get_result.value.contains("ステータス") ||
                 get_result.value.contains("サービス"))
  }
  
  // 测试参数化资源
  I18nResourceManager::set_current_locale(i18n_manager, "en-US")
  
  let param_key = "telemetry.span.duration"
  let params = [
    ("span_name", "http.request"),
    ("duration_ms", "250"),
    ("unit", "ms")
  ]
  
  let param_result = I18nResourceManager::get_formatted_string(i18n_manager, param_key, params)
  assert_true(param_result.success)
  assert_true(param_result.value.contains("http.request"))
  assert_true(param_result.value.contains("250"))
  
  // 测试复数形式
  I18nResourceManager::set_current_locale(i18n_manager, "en-US")
  
  let plural_key = "telemetry.spans.count"
  let plural_params = [("count", "1")]
  let singular_result = I18nResourceManager::get_plural_string(i18n_manager, plural_key, plural_params)
  assert_true(singular_result.success)
  assert_true(singular_result.value.contains("span"))
  
  let plural_params_plural = [("count", "5")]
  let plural_result = I18nResourceManager::get_plural_string(i18n_manager, plural_key, plural_params_plural)
  assert_true(plural_result.success)
  assert_true(plural_result.value.contains("spans"))
  
  // 测试中文复数形式（中文没有复数变化）
  I18nResourceManager::set_current_locale(i18n_manager, "zh-CN")
  
  let zh_singular_result = I18nResourceManager::get_plural_string(i18n_manager, plural_key, plural_params)
  assert_true(zh_singular_result.success)
  
  let zh_plural_result = I18nResourceManager::get_plural_string(i18n_manager, plural_key, plural_params_plural)
  assert_true(zh_plural_result.success)
  
  // 中文通常单复数形式相同
  assert_eq(zh_singular_result.value, zh_plural_result.value)
  
  // 测试资源回退机制
  I18nResourceManager::set_current_locale(i18n_manager, "unsupported-locale")
  
  // 应该回退到默认语言
  let fallback_result = I18nResourceManager::get_string(i18n_manager, "telemetry.span.name")
  assert_true(fallback_result.success)
  
  // 验证回退到默认语言
  I18nResourceManager::set_current_locale(i18n_manager, "en-US")
  let default_result = I18nResourceManager::get_string(i18n_manager, "telemetry.span.name")
  assert_eq(fallback_result.value, default_result.value)
  
  // 测试资源热重载
  let reload_result = I18nResourceManager::reload_locale(i18n_manager, "en-US")
  assert_true(reload_result.success)
  
  // 验证重载后的资源仍然可用
  let post_reload_result = I18nResourceManager::get_string(i18n_manager, "telemetry.span.name")
  assert_true(post_reload_result.success)
  
  // 测试资源缓存
  let cache_stats = I18nResourceManager::get_cache_stats(i18n_manager)
  assert_true(cache_stats.total_entries > 0)
  assert_true(cache_stats.hit_rate >= 0.0)
  assert_true(cache_stats.hit_rate <= 1.0)
  
  // 清理资源
  let shutdown_result = I18nResourceManager::shutdown(i18n_manager)
  assert_true(shutdown_result.success)
}

// 测试2: 日期和时间格式化
test "日期和时间格式化测试" {
  // 创建日期时间格式化器
  let date_formatter = DateTimeFormatter::new()
  
  // 测试不同地区的日期格式
  let date_formats = [
    ("en-US", "MM/dd/yyyy"),      // 美国格式: 01/04/2026
    ("zh-CN", "yyyy年MM月dd日"),  // 中国格式: 2026年01月04日
    ("ja-JP", "yyyy/MM/dd"),      // 日本格式: 2026/01/04
    ("fr-FR", "dd/MM/yyyy"),      // 法国格式: 04/01/2026
    ("de-DE", "dd.MM.yyyy"),      // 德国格式: 04.01.2026
    ("es-ES", "dd/MM/yyyy"),      // 西班牙格式: 04/01/2026
    ("ru-RU", "dd.MM.yyyy"),      // 俄语格式: 04.01.2026
    ("pt-BR", "dd/MM/yyyy")       // 巴西格式: 04/01/2026
  ]
  
  let test_timestamp = 1640995200000 // 2022-01-01 00:00:00 UTC
  
  for (locale, expected_pattern) in date_formats {
    DateTimeFormatter::set_locale(date_formatter, locale)
    
    let format_result = DateTimeFormatter::format_date(date_formatter, test_timestamp)
    assert_true(format_result.success)
    
    let formatted_date = format_result.formatted_string
    assert_true(formatted_date.length() > 0)
    
    // 验证格式符合预期
    if locale == "en-US" {
      assert_true(formatted_date.contains("01/04/2026"))
    } else if locale == "zh-CN" {
      assert_true(formatted_date.contains("2026年01月04日"))
    } else if locale == "ja-JP" {
      assert_true(formatted_date.contains("2026/01/04"))
    }
  }
  
  // 测试时间格式
  let time_formats = [
    ("en-US", "h:mm:ss a"),        // 美国格式: 12:00:00 AM
    ("zh-CN", "HH:mm:ss"),         // 中国格式: 00:00:00
    ("ja-JP", "HH:mm:ss"),         // 日本格式: 00:00:00
    ("fr-FR", "HH:mm:ss"),         // 法国格式: 00:00:00
    ("de-DE", "HH:mm:ss"),         // 德国格式: 00:00:00
    ("es-ES", "HH:mm:ss"),         // 西班牙格式: 00:00:00
    ("ru-RU", "HH:mm:ss"),         // 俄语格式: 00:00:00
    ("pt-BR", "HH:mm:ss")          // 巴西格式: 00:00:00
  ]
  
  for (locale, expected_pattern) in time_formats {
    DateTimeFormatter::set_locale(date_formatter, locale)
    
    let format_result = DateTimeFormatter::format_time(date_formatter, test_timestamp)
    assert_true(format_result.success)
    
    let formatted_time = format_result.formatted_string
    assert_true(formatted_time.length() > 0)
    
    // 验证时间格式
    if locale == "en-US" {
      assert_true(formatted_time.contains("12:00:00 AM") || formatted_time.contains("00:00:00"))
    } else if locale == "zh-CN" {
      assert_true(formatted_time.contains("00:00:00"))
    }
  }
  
  // 测试日期时间组合格式
  DateTimeFormatter::set_locale(date_formatter, "en-US")
  let datetime_result = DateTimeFormatter::format_datetime(date_formatter, test_timestamp)
  assert_true(datetime_result.success)
  assert_true(datetime_result.formatted_string.length() > 0)
  
  // 测试相对时间格式
  let now_timestamp = 1640995200000
  let past_timestamp = now_timestamp - 3600000 // 1小时前
  
  let relative_result = DateTimeFormatter::format_relative(date_formatter, past_timestamp, now_timestamp)
  assert_true(relative_result.success)
  assert_true(relative_result.formatted_string.contains("hour") || relative_result.formatted_string.contains("小时"))
  
  // 测试时区处理
  let timezones = [
    ("UTC", 0),
    ("America/New_York", -5),
    ("Europe/London", 0),
    ("Asia/Shanghai", 8),
    ("Asia/Tokyo", 9),
    ("America/Los_Angeles", -8)
  ]
  
  for (timezone_name, offset) in timezones {
    DateTimeFormatter::set_timezone(date_formatter, timezone_name)
    
    let timezone_result = DateTimeFormatter::format_datetime(date_formatter, test_timestamp)
    assert_true(timezone_result.success)
    
    // 验证时区信息
    let timezone_info = timezone_result.timezone_info
    assert_eq(timezone_info.name, timezone_name)
    assert_eq(timezone_info.offset, offset)
  }
  
  // 测试本地化月份和星期名称
  let month_names = [
    ("en-US", ["January", "February", "March", "April", "May", "June", 
               "July", "August", "September", "October", "November", "December"]),
    ("zh-CN", ["一月", "二月", "三月", "四月", "五月", "六月", 
               "七月", "八月", "九月", "十月", "十一月", "十二月"]),
    ("ja-JP", ["1月", "2月", "3月", "4月", "5月", "6月", 
               "7月", "8月", "9月", "10月", "11月", "12月"])
  ]
  
  for (locale, expected_months) in month_names {
    DateTimeFormatter::set_locale(date_formatter, locale)
    
    for i in 0..=11 {
      let month_result = DateTimeFormatter::get_month_name(date_formatter, i)
      assert_true(month_result.success)
      assert_eq(month_result.month_name, expected_months[i])
    }
  }
  
  // 测试本地化星期名称
  let day_names = [
    ("en-US", ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]),
    ("zh-CN", ["星期日", "星期一", "星期二", "星期三", "星期四", "星期五", "星期六"]),
    ("ja-JP", ["日曜日", "月曜日", "火曜日", "水曜日", "木曜日", "金曜日", "土曜日"])
  ]
  
  for (locale, expected_days) in day_names {
    DateTimeFormatter::set_locale(date_formatter, locale)
    
    for i in 0..=6 {
      let day_result = DateTimeFormatter::get_day_name(date_formatter, i)
      assert_true(day_result.success)
      assert_eq(day_result.day_name, expected_days[i])
    }
  }
  
  // 测试日期解析
  DateTimeFormatter::set_locale(date_formatter, "en-US")
  
  let parse_result = DateTimeFormatter::parse_date(date_formatter, "01/04/2026")
  assert_true(parse_result.success)
  assert_eq(parse_result.timestamp, test_timestamp)
  
  // 测试日期时间解析
  let datetime_parse_result = DateTimeFormatter::parse_datetime(date_formatter, "01/04/2026 12:00:00 AM")
  assert_true(datetime_parse_result.success)
  
  // 验证自定义格式
  DateTimeFormatter::set_custom_format(date_formatter, "yyyy-MM-dd'T'HH:mm:ss")
  let custom_result = DateTimeFormatter::format_datetime(date_formatter, test_timestamp)
  assert_true(custom_result.success)
  assert_true(custom_result.formatted_string.contains("2022-01-01"))
}

// 测试3: 数字和货币格式化
test "数字和货币格式化测试" {
  // 创建数字格式化器
  let number_formatter = NumberFormatter::new()
  
  // 测试不同地区的数字格式
  let number_formats = [
    ("en-US", "1,234,567.89"),    // 美国格式: 逗号分隔，点号小数点
    ("zh-CN", "1,234,567.89"),    // 中国格式: 逗号分隔，点号小数点
    ("de-DE", "1.234.567,89"),    // 德国格式: 点号分隔，逗号小数点
    ("fr-FR", "1 234 567,89"),    // 法国格式: 空格分隔，逗号小数点
    ("ja-JP", "1,234,567.89"),    // 日本格式: 逗号分隔，点号小数点
    ("ru-RU", "1 234 567,89"),    // 俄语格式: 空格分隔，逗号小数点
    ("es-ES", "1.234.567,89"),    // 西班牙格式: 点号分隔，逗号小数点
    ("pt-BR", "1.234.567,89")     // 巴西格式: 点号分隔，逗号小数点
  ]
  
  let test_number = 1234567.89
  
  for (locale, expected_format) in number_formats {
    NumberFormatter::set_locale(number_formatter, locale)
    
    let format_result = NumberFormatter::format(number_formatter, test_number)
    assert_true(format_result.success)
    
    let formatted_number = format_result.formatted_string
    assert_true(formatted_number.length() > 0)
    
    // 验证格式符合预期
    if locale == "en-US" || locale == "zh-CN" || locale == "ja-JP" {
      assert_true(formatted_number.contains("1,234,567.89"))
    } else if locale == "de-DE" || locale == "es-ES" || locale == "pt-BR" {
      assert_true(formatted_number.contains("1.234.567,89"))
    } else if locale == "fr-FR" || locale == "ru-RU" {
      assert_true(formatted_number.contains("1 234 567,89"))
    }
  }
  
  // 测试百分比格式
  NumberFormatter::set_locale(number_formatter, "en-US")
  NumberFormatter::set_percent_style(number_formatter, true)
  
  let percent_result = NumberFormatter::format(number_formatter, 0.7543)
  assert_true(percent_result.success)
  assert_true(percent_result.formatted_string.contains("%"))
  
  // 测试科学计数法
  NumberFormatter::set_scientific_style(number_formatter, true)
  
  let scientific_result = NumberFormatter::format(number_formatter, 1234567.89)
  assert_true(scientific_result.success)
  assert_true(scientific_result.formatted_string.contains("E"))
  
  // 测试货币格式
  let currency_formatter = CurrencyFormatter::new()
  
  let currency_formats = [
    ("en-US", "USD", "$1,234,567.89"),      // 美元
    ("zh-CN", "CNY", "¥1,234,567.89"),      // 人民币
    ("ja-JP", "JPY", "¥1,234,567"),          // 日元（无小数）
    ("de-DE", "EUR", "1.234.567,89 €"),      // 欧元
    ("fr-FR", "EUR", "1 234 567,89 €"),      // 欧元
    ("ru-RU", "RUB", "1 234 567,89 ₽"),      // 卢布
    ("es-ES", "EUR", "1.234.567,89 €"),      // 欧元
    ("pt-BR", "BRL", "R$ 1.234.567,89")      // 巴西雷亚尔
  ]
  
  for (locale, currency_code, expected_format) in currency_formats {
    CurrencyFormatter::set_locale(currency_formatter, locale)
    CurrencyFormatter::set_currency_code(currency_formatter, currency_code)
    
    let format_result = CurrencyFormatter::format(currency_formatter, test_number)
    assert_true(format_result.success)
    
    let formatted_currency = format_result.formatted_string
    assert_true(formatted_currency.length() > 0)
    
    // 验证货币符号
    if currency_code == "USD" {
      assert_true(formatted_currency.contains("$"))
    } else if currency_code == "CNY" {
      assert_true(formatted_currency.contains("¥"))
    } else if currency_code == "JPY" {
      assert_true(formatted_currency.contains("¥"))
    } else if currency_code == "EUR" {
      assert_true(formatted_currency.contains("€"))
    } else if currency_code == "RUB" {
      assert_true(formatted_currency.contains("₽"))
    } else if currency_code == "BRL" {
      assert_true(formatted_currency.contains("R$"))
    }
  }
  
  // 测试单位格式化
  let unit_formatter = UnitFormatter::new()
  
  let unit_tests = [
    ("en-US", "length", "meter", 1234.5, "1,234.5 m"),
    ("zh-CN", "length", "meter", 1234.5, "1,234.5 米"),
    ("ja-JP", "length", "meter", 1234.5, "1,234.5 メートル"),
    ("en-US", "weight", "kilogram", 75.2, "75.2 kg"),
    ("zh-CN", "weight", "kilogram", 75.2, "75.2 千克"),
    ("ja-JP", "weight", "kilogram", 75.2, "75.2 キログラム"),
    ("en-US", "temperature", "celsius", 25.5, "25.5°C"),
    ("zh-CN", "temperature", "celsius", 25.5, "25.5°C"),
    ("ja-JP", "temperature", "celsius", 25.5, "25.5°C")
  ]
  
  for (locale, unit_type, unit_name, value, expected_format) in unit_tests {
    UnitFormatter::set_locale(unit_formatter, locale)
    
    let format_result = UnitFormatter::format(unit_formatter, value, unit_type, unit_name)
    assert_true(format_result.success)
    
    let formatted_unit = format_result.formatted_string
    assert_true(formatted_unit.length() > 0)
    
    // 验证单位符号
    if unit_type == "length" && unit_name == "meter" {
      if locale == "en-US" {
        assert_true(formatted_unit.contains("m"))
      } else if locale == "zh-CN" {
        assert_true(formatted_unit.contains("米"))
      } else if locale == "ja-JP" {
        assert_true(formatted_unit.contains("メートル"))
      }
    }
  }
  
  // 测试数字解析
  NumberFormatter::set_locale(number_formatter, "en-US")
  NumberFormatter::set_decimal_style(number_formatter, true)
  
  let parse_result = NumberFormatter::parse(number_formatter, "1,234,567.89")
  assert_true(parse_result.success)
  assert_true(parse_result.value - test_number < 0.001)
  
  // 测试大数字格式化
  let large_number = 1234567890123456.78
  
  NumberFormatter::set_locale(number_formatter, "en-US")
  let large_result = NumberFormatter::format_large(number_formatter, large_number)
  assert_true(large_result.success)
  
  // 测试小数精度控制
  NumberFormatter::set_decimal_places(number_formatter, 2)
  
  let precision_result = NumberFormatter::format(number_formatter, 3.14159265)
  assert_true(precision_result.success)
  assert_true(precision_result.formatted_string.contains("3.14"))
  
  // 测试数字分组
  NumberFormatter::set_grouping_used(number_formatter, true)
  NumberFormatter::set_grouping_size(number_formatter, 3)
  
  let grouping_result = NumberFormatter::format(number_formatter, 12345678)
  assert_true(grouping_result.success)
  assert_true(grouping_result.formatted_string.contains("12,345,678"))
}

// 测试4: 文本方向和书写系统
test "文本方向和书写系统测试" {
  // 创建文本方向处理器
  let text_direction = TextDirectionHandler::new()
  
  // 测试从左到右(LTR)文本
  let ltr_text = "Azimuth Telemetry System"
  let ltr_result = TextDirectionHandler::analyze_direction(text_direction, ltr_text)
  assert_true(ltr_result.success)
  assert_eq(ltr_result.direction, TextDirection::LTR)
  assert_eq(ltr_result.base_direction, TextDirection::LTR)
  
  // 测试从右到左(RTL)文本
  let rtl_text = "نظام قياس الأداء عن بعد"
  let rtl_result = TextDirectionHandler::analyze_direction(text_direction, rtl_text)
  assert_true(rtl_result.success)
  assert_eq(rtl_result.direction, TextDirection::RTL)
  assert_eq(rtl_result.base_direction, TextDirection::RTL)
  
  // 测试混合方向文本
  let mixed_text = "Azimuth نظام قياس الأداء"
  let mixed_result = TextDirectionHandler::analyze_direction(text_direction, mixed_text)
  assert_true(mixed_result.success)
  assert_true(mixed_result.has_mixed_direction)
  
  // 测试阿拉伯语本地化
  let arabic_formatter = ArabicTextFormatter::new()
  
  let arabic_numbers = [1, 2, 3, 10, 100, 1000]
  let arabic_number_result = ArabicTextFormatter::format_numbers(arabic_formatter, arabic_numbers)
  assert_true(arabic_number_result.success)
  
  // 验证阿拉伯数字
  let formatted_arabic = arabic_number_result.formatted_text
  assert_true(formatted_arabic.contains("١") || formatted_arabic.contains("٢") || 
               formatted_arabic.contains("٣") || formatted_arabic.contains("١٠"))
  
  // 测试希伯来语文本
  let hebrew_text = "מערכת טלמטריה מרוחקת"
  let hebrew_result = TextDirectionHandler::analyze_direction(text_direction, hebrew_text)
  assert_true(hebrew_result.success)
  assert_eq(hebrew_result.direction, TextDirection::RTL)
  
  // 测试双向文本渲染
  let bidi_renderer = BidiTextRenderer::new()
  
  let bidi_text = "Hello مرحبا World"
  let bidi_result = BidiTextRenderer::render(bidi_renderer, bidi_text, TextDirection::LTR)
  assert_true(bidi_result.success)
  
  let rendered_text = bidi_result.rendered_text
  assert_true(rendered_text.length() > 0)
  
  // 测试文本镜像（用于RTL布局）
  let mirror_result = TextDirectionHandler::mirror_text(text_direction, ltr_text, TextDirection::RTL)
  assert_true(mirror_result.success)
  assert_true(mirror_result.mirrored_text.length() > 0)
  
  // 测试垂直书写系统
  let vertical_formatter = VerticalTextFormatter::new()
  
  let vertical_text = "縦書きテキスト" // 日文垂直文本
  let vertical_result = VerticalTextFormatter::format(vertical_formatter, vertical_text, VerticalDirection::TopToBottom)
  assert_true(vertical_result.success)
  
  // 测试中日韩文本处理
  let cjk_formatter = CJKTextFormatter::new()
  
  let cjk_texts = [
    ("zh-CN", "遥测系统性能监控"),
    ("ja-JP", "テレメトリシステム性能監視"),
    ("ko-KR", "원격 측정 시스템 성능 모니터링")
  ]
  
  for (locale, text) in cjk_texts {
    CJKTextFormatter::set_locale(cjk_formatter, locale)
    
    let format_result = CJKTextFormatter::format(cjk_formatter, text)
    assert_true(format_result.success)
    
    let formatted_cjk = format_result.formatted_text
    assert_true(formatted_cjk.length() > 0)
    
    // 验证文本属性
    let text_properties = format_result.text_properties
    assert_true(text_properties.is_cjk)
    assert_true(text_properties.line_break_rule == LineBreakRule::CJK)
  }
  
  // 测试文本对齐
  let alignment_formatter = TextAlignmentFormatter::new()
  
  let alignment_tests = [
    (TextDirection::LTR, TextAlignment::Left, "Left aligned LTR text"),
    (TextDirection::LTR, TextAlignment::Right, "Right aligned LTR text"),
    (TextDirection::LTR, TextAlignment::Center, "Center aligned LTR text"),
    (TextDirection::RTL, TextAlignment::Left, "Left aligned RTL text"),
    (TextDirection::RTL, TextAlignment::Right, "Right aligned RTL text"),
    (TextDirection::RTL, TextAlignment::Center, "Center aligned RTL text")
  ]
  
  for (direction, alignment, text) in alignment_tests {
    let align_result = TextAlignmentFormatter::align(alignment_formatter, text, direction, alignment)
    assert_true(align_result.success)
    
    let aligned_text = align_result.aligned_text
    assert_true(aligned_text.length() > 0)
  }
  
  // 测试文本换行
  let line_breaker = LineBreaker::new()
  
  LineBreaker::set_locale(line_breaker, "zh-CN")
  LineBreaker::set_max_width(line_breaker, 20)
  
  let long_cjk_text = "这是一个很长的中文文本需要正确地进行换行处理以确保可读性"
  let break_result = LineBreaker::break_text(line_breaker, long_cjk_text)
  assert_true(break_result.success)
  
  let broken_lines = break_result.lines
  assert_true(broken_lines.length() > 1) // 应该被分成多行
  
  // 测试标点符号处理
  let punctuation_handler = PunctuationHandler::new()
  
  let punctuation_tests = [
    ("en-US", "Hello, world! How are you?"),
    ("zh-CN", "你好，世界！你好吗？"),
    ("ja-JP", "こんにちは、世界！元気ですか？"),
    ("ar-SA", "مرحبا بالعالم! كيف حالك؟")
  ]
  
  for (locale, text) in punctuation_tests {
    PunctuationHandler::set_locale(punctuation_handler, locale)
    
    let punct_result = PunctuationHandler::process(punctuation_handler, text)
    assert_true(punct_result.success)
    
    let processed_text = punct_result.processed_text
    assert_true(processed_text.length() > 0)
    
    // 验证标点符号间距
    if locale == "zh-CN" || locale == "ja-JP" {
      // 中日文通常不需要标点符号后的空格
      assert_true(!processed_text.contains("， ") || !processed_text.contains("！ "))
    } else if locale == "en-US" {
      // 英文通常需要标点符号后的空格
      assert_true(processed_text.contains(", ") || processed_text.contains("! "))
    }
  }
}

// 测试5: 文化特定格式和约定
test "文化特定格式和约定测试" {
  // 创建文化格式化器
  let cultural_formatter = CulturalFormatter::new()
  
  // 测试姓名格式
  let name_formats = [
    ("en-US", "John Doe", "First Last"),
    ("zh-CN", "张三", "姓 名"),
    ("ja-JP", "田中 太郎", "姓 名"),
    ("ko-KR", "김철수", "姓 名"),
    ("hu-HU", "Nagy János", "Last First"),
    ("es-ES", "Juan García", "First Last")
  ]
  
  for (locale, name, expected_format) in name_formats {
    CulturalFormatter::set_locale(cultural_formatter, locale)
    
    let name_result = CulturalFormatter::format_name(cultural_formatter, name)
    assert_true(name_result.success)
    
    let formatted_name = name_result.formatted_name
    assert_true(formatted_name.length() > 0)
    
    let name_info = name_result.name_info
    assert_eq(name_info.format_type, expected_format)
  }
  
  // 测试地址格式
  let address_formatter = AddressFormatter::new()
  
  let address_data = {
    "street": "123 Main St",
    "city": "New York",
    "state": "NY",
    "postal_code": "10001",
    "country": "United States"
  }
  
  let address_formats = [
    ("en-US", "123 Main St\nNew York, NY 10001\nUnited States"),
    ("zh-CN", "美国\n纽约州纽约市\n主街123号\n邮编: 10001"),
    ("ja-JP", "〒10001\nニューヨーク州ニューヨーク市\nメインストリート123番地\nアメリカ合衆国"),
    ("de-DE", "123 Main St\n10001 New York, NY\nUnited States"),
    ("fr-FR", "123 Main St\n10001 New York, NY\nUnited States")
  ]
  
  for (locale, expected_pattern) in address_formats {
    AddressFormatter::set_locale(address_formatter, locale)
    
    let format_result = AddressFormatter::format(address_formatter, address_data)
    assert_true(format_result.success)
    
    let formatted_address = format_result.formatted_address
    assert_true(formatted_address.length() > 0)
    
    // 验证地址格式符合地区习惯
    if locale == "zh-CN" {
      assert_true(formatted_address.contains("美国") && formatted_address.contains("邮编"))
    } else if locale == "ja-JP" {
      assert_true(formatted_address.contains("〒") && formatted_address.contains("番地"))
    }
  }
  
  // 测试电话号码格式
  let phone_formatter = PhoneFormatter::new()
  
  let phone_number = "+12125551234"
  
  let phone_formats = [
    ("en-US", "+1 (212) 555-1234"),
    ("zh-CN", "+1 212-555-1234"),
    ("ja-JP", "+1-212-555-1234"),
    ("de-DE", "+1 212 5551234"),
    ("fr-FR", "+1 212 55 12 34")
  ]
  
  for (locale, expected_format) in phone_formats {
    PhoneFormatter::set_locale(phone_formatter, locale)
    
    let format_result = PhoneFormatter::format(phone_formatter, phone_number)
    assert_true(format_result.success)
    
    let formatted_phone = format_result.formatted_phone
    assert_true(formatted_phone.length() > 0)
  }
  
  // 测试度量衡系统
  let measurement_formatter = MeasurementFormatter::new()
  
  let measurement_tests = [
    ("en-US", MeasurementSystem::Imperial, "5.5 feet", "1.68 meters"),
    ("zh-CN", MeasurementSystem::Metric, "1.68 米", "5.51 feet"),
    ("ja-JP", MeasurementSystem::Metric, "1.68 メートル", "5.51 フィート"),
    ("de-DE", MeasurementSystem::Metric, "1,68 Meter", "5,51 Fuß"),
    ("fr-FR", MeasurementSystem::Metric, "1,68 mètre", "5,51 pied")
  ]
  
  let height_in_meters = 1.68
  
  for (locale, system, expected_metric, expected_imperial) in {
    MeasurementFormatter::set_locale(measurement_formatter, locale)
    MeasurementFormatter::set_system(measurement_formatter, system)
    
    let format_result = MeasurementFormatter::format_length(measurement_formatter, height_in_meters, "meter")
    assert_true(format_result.success)
    
    let formatted_length = format_result.formatted_length
    assert_true(formatted_length.length() > 0)
    
    if system == MeasurementSystem::Metric {
      assert_true(formatted_length.contains("1.68") || formatted_length.contains("1,68"))
    }
  }
  
  // 测试纸张尺寸
  let paper_formatter = PaperSizeFormatter::new()
  
  let paper_sizes = [
    ("en-US", "letter", "8.5 x 11 inches"),
    ("en-US", "legal", "8.5 x 14 inches"),
    ("zh-CN", "a4", "210 x 297 mm"),
    ("ja-JP", "a4", "210 x 297 mm"),
    ("de-DE", "a4", "210 x 297 mm"),
    ("fr-FR", "a4", "210 x 297 mm")
  ]
  
  for (locale, size_name, expected_format) in paper_sizes {
    PaperSizeFormatter::set_locale(paper_formatter, locale)
    
    let format_result = PaperSizeFormatter::format(paper_formatter, size_name)
    assert_true(format_result.success)
    
    let formatted_size = format_result.formatted_size
    assert_true(formatted_size.length() > 0)
  }
  
  // 测试颜色命名
  let color_formatter = ColorFormatter::new()
  
  let color_tests = [
    ("en-US", ["red", "blue", "green", "yellow", "black", "white"]),
    ("zh-CN", ["红色", "蓝色", "绿色", "黄色", "黑色", "白色"]),
    ("ja-JP", ["赤", "青", "緑", "黄", "黒", "白"]),
    ("de-DE", ["rot", "blau", "grün", "gelb", "schwarz", "weiß"]),
    ("fr-FR", ["rouge", "bleu", "vert", "jaune", "noir", "blanc"])
  ]
  
  for (locale, expected_colors) in color_tests {
    ColorFormatter::set_locale(color_formatter, locale)
    
    for (i, color_name) in expected_colors.enumerate() {
      let format_result = ColorFormatter::format(color_formatter, color_name)
      assert_true(format_result.success)
      
      let formatted_color = format_result.formatted_color
      assert_true(formatted_color.length() > 0)
      
      if i < 3 { // 测试前3个颜色
        let color_info = format_result.color_info
        assert_true(color_info.name.length() > 0)
      }
    }
  }
  
  // 测试首字母排序规则
  let collation_formatter = CollationFormatter::new()
  
  let collation_tests = [
    ("en-US", ["apple", "banana", "cherry", "date"], ["apple", "banana", "cherry", "date"]),
    ("zh-CN", ["苹果", "香蕉", "樱桃", "枣"], ["枣", "苹果", "香蕉", "樱桃"]),
    ("ja-JP", ["りんご", "ばなな", "さくらんぼ", "なつめ"], ["ばなな", "さくらんぼ", "なつめ", "りんご"])
  ]
  
  for (locale, input_words, expected_order) in collation_tests {
    CollationFormatter::set_locale(collation_formatter, locale)
    
    let sort_result = CollationFormatter::sort_words(collation_formatter, input_words)
    assert_true(sort_result.success)
    
    let sorted_words = sort_result.sorted_words
    assert_eq(sorted_words.length(), input_words.length())
    
    // 验证排序符合地区习惯
    for i in 0..=expected_order.length() - 1 {
      assert_eq(sorted_words[i], expected_order[i])
    }
  }
  
  // 测试姓名称谓
  let title_formatter = TitleFormatter::new()
  
  let title_tests = [
    ("en-US", "male", "Mr.", "female", "Ms.", "neutral", "Mx."),
    ("zh-CN", "male", "先生", "female", "女士", "neutral", "君"),
    ("ja-JP", "male", "様", "female", "様", "neutral", "様"),
    ("de-DE", "male", "Herr", "female", "Frau", "neutral", "Mx."),
    ("fr-FR", "male", "M.", "female", "Mme", "neutral", "M.")
  ]
  
  for (locale, gender, male_title, female_title, neutral_gender, neutral_title) in title_tests {
    TitleFormatter::set_locale(title_formatter, locale)
    
    let male_result = TitleFormatter::get_title(title_formatter, gender)
    assert_true(male_result.success)
    assert_eq(male_result.title, male_title)
    
    let female_result = TitleFormatter::get_title(title_formatter, "female")
    assert_true(female_result.success)
    assert_eq(female_result.title, female_title)
    
    let neutral_result = TitleFormatter::get_title(title_formatter, neutral_gender)
    assert_true(neutral_result.success)
    assert_eq(neutral_result.title, neutral_title)
  }
}

// 测试6: 字符编码和转换
test "字符编码和转换测试" {
  // 创建字符编码转换器
  let encoding_converter = EncodingConverter::new()
  
  // 测试多字节字符编码
  let unicode_texts = [
    "中文测试文本",
    "日本語テストテキスト",
    "한국어 테스트 텍스트",
    "العربية اختبار النص",
    "עברית טקסט בדיקה",
    "Тестовый текст на русском",
    "Текст на беларускай мове",
    "Ελληνικό κείμενο δοκιμής",
    "Текст кирилице"
  ]
  
  for text in unicode_texts {
    // 转换为UTF-8字节
    let utf8_result = EncodingConverter::to_utf8(encoding_converter, text)
    assert_true(utf8_result.success)
    
    let utf8_bytes = utf8_result.bytes
    assert_true(utf8_bytes.length() > 0)
    
    // 从UTF-8字节转换回文本
    let from_utf8_result = EncodingConverter::from_utf8(encoding_converter, utf8_bytes)
    assert_true(from_utf8_result.success)
    assert_eq(from_utf8_result.text, text)
    
    // 转换为UTF-16
    let utf16_result = EncodingConverter::to_utf16(encoding_converter, text)
    assert_true(utf16_result.success)
    
    let utf16_bytes = utf16_result.bytes
    assert_true(utf16_bytes.length() > 0)
    
    // 从UTF-16字节转换回文本
    let from_utf16_result = EncodingConverter::from_utf16(encoding_converter, utf16_bytes)
    assert_true(from_utf16_result.success)
    assert_eq(from_utf16_result.text, text)
  }
  
  // 测试字符集检测
  let mixed_encoding_data = "Hello 世界 こんにちは Привет".to_bytes()
  let detect_result = EncodingConverter::detect_encoding(encoding_converter, mixed_encoding_data)
  assert_true(detect_result.success)
  
  let detected_encoding = detect_result.encoding
  assert_true(detected_encoding == "UTF-8" || detected_encoding == "UTF-16")
  
  // 测试字符规范化
  let normalizer = UnicodeNormalizer::new()
  
  let test_strings = [
    ("café", "café"), // 带重音符和不带重音符的组合
    ("你好", "你好"),       // 中文（无变化）
    ("👍", "👍"),          // Emoji（无变化）
    ("é", "é")        // 组合字符和预组合字符
  ]
  
  for (composed, decomposed) in test_strings {
    // NFC规范化（规范形式C）
    let nfc_result = UnicodeNormalizer::normalize(normalizer, composed, NormalizationForm::NFC)
    assert_true(nfc_result.success)
    
    // NFD规范化（规范形式D）
    let nfd_result = UnicodeNormalizer::normalize(normalizer, decomposed, NormalizationForm::NFD)
    assert_true(nfd_result.success)
    
    // 验证规范化结果
    let nfc_normalized = nfc_result.normalized_text
    let nfd_normalized = nfd_result.normalized_text
    
    // NFC应该产生预组合字符，NFD应该产生分解字符
    if composed.contains("é") {
      assert_true(nfc_normalized.contains("é"))
    }
    
    if decomposed.contains("é") {
      assert_true(nfd_normalized.contains("é"))
    }
  }
  
  // 测试字符转义
  let escape_handler = UnicodeEscapeHandler::new()
  
  let escape_tests = [
    ("Hello\nWorld", "Hello\\nWorld"),
    ("Tab\tCharacter", "Tab\\tCharacter"),
    ("Quote\"Character", "Quote\\\"Character"),
    ("Backslash\\Character", "Backslash\\\\Character"),
    ("Unicode中文", "Unicode\\u4E2D\\u6587")
  ]
  
  for (original, expected_escaped) in escape_tests {
    let escape_result = UnicodeEscapeHandler::escape(escape_handler, original)
    assert_true(escape_result.success)
    assert_eq(escape_result.escaped_text, expected_escaped)
    
    // 反向转义
    let unescape_result = UnicodeEscapeHandler::unescape(escape_handler, expected_escaped)
    assert_true(unescape_result.success)
    assert_eq(unescape_result.unescaped_text, original)
  }
  
  // 测试字符大小写转换
  let case_converter = CaseConverter::new()
  
  let case_tests = [
    ("en-US", "hello world", "HELLO WORLD", "Hello World"),
    ("zh-CN", "hello world", "HELLO WORLD", "Hello World"), // 中文无大小写
    ("tr-TR", "istanbul", "İSTANBUL", "İstanbul"), // 土耳其语特殊大小写
    ("el-GR", "άλφα", "ΆΛΦΑ", "Άλφα") // 希腊语
  ]
  
  for (locale, lower, upper, title) in case_tests {
    CaseConverter::set_locale(case_converter, locale)
    
    let lower_result = CaseConverter::to_upper(case_converter, lower)
    assert_true(lower_result.success)
    
    let upper_result = CaseConverter::to_lower(case_converter, upper)
    assert_true(upper_result.success)
    
    let title_result = CaseConverter::to_title(case_converter, title)
    assert_true(title_result.success)
  }
  
  // 测试字符宽度（全角和半角）
  let width_handler = CharacterWidthHandler::new()
  
  let width_tests = [
    ("ABC123", "ＡＢＣ１２３"), // 半角转全角
    ("ＡＢＣ１２３", "ABC123"), // 全角转半角
    ("hello", "ｈｅｌｌｏ"),   // 半角转全角
    ("ｈｅｌｌｏ", "hello")    // 全角转半角
  ]
  
  for (half_width, full_width) in width_tests {
    let to_full_result = CharacterWidthHandler::to_full_width(width_handler, half_width)
    assert_true(to_full_result.success)
    assert_eq(to_full_result.converted_text, full_width)
    
    let to_half_result = CharacterWidthHandler::to_half_width(width_handler, full_width)
    assert_true(to_half_result.success)
    assert_eq(to_half_result.converted_text, half_width)
  }
  
  // 测试字符显示宽度计算
  let display_width_tests = [
    ("ABC", 3),          // ASCII字符，宽度1
    ("中文", 4),          // 中文字符，宽度2
    ("👍", 2),            // Emoji，宽度2
    ("A中文B👍C", 10)     // 混合字符
  ]
  
  for (text, expected_width) in display_width_tests {
    let width_result = CharacterWidthHandler::get_display_width(width_handler, text)
    assert_true(width_result.success)
    assert_eq(width_result.width, expected_width)
  }
  
  // 测试字符过滤
  let character_filter = CharacterFilter::new()
  
  let filter_tests = [
    ("letters", "Hello123World", "HelloWorld"),
    ("digits", "Hello123World", "123"),
    ("alphanumeric", "Hello-123-World!", "Hello123World"),
    ("cjk", "Hello中文World", "中文"),
    ("arabic", "HelloمرحباWorld", "مرحبا")
  ]
  
  for (filter_type, input, expected_output) in filter_tests {
    let filter_result = CharacterFilter::filter(character_filter, input, filter_type)
    assert_true(filter_result.success)
    assert_eq(filter_result.filtered_text, expected_output)
  }
  
  // 测试字符替换
  let character_replacer = CharacterReplacer::new()
  
  let replacement_rules = [
    ("old", "new"),
    ("123", "456"),
    ("中文", "Chinese"),
    ("テスト", "test")
  ]
  
  let input_text = "old 123 中文 テスト"
  let expected_output = "new 456 Chinese test"
  
  for (old_text, new_text) in replacement_rules {
    CharacterReplacer::add_rule(character_replacer, old_text, new_text)
  }
  
  let replace_result = CharacterReplacer::replace(character_replacer, input_text)
  assert_true(replace_result.success)
  assert_eq(replace_result.replaced_text, expected_output)
  
  // 测试字符统计
  let character_stats = CharacterStats::new()
  
  let stats_input = "Hello你好World世界123!@#"
  let stats_result = CharacterStats::analyze(character_stats, stats_input)
  assert_true(stats_result.success)
  
  let stats = stats_result.statistics
  assert_true(stats.total_characters > 0)
  assert_true(stats.ascii_characters > 0)
  assert_true(stats.cjk_characters > 0)
  assert_true(stats.digits > 0)
  assert_true(stats.symbols > 0)
}