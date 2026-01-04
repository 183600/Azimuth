// Azimuth 国际化综合支持测试用例
// 专注于遥测系统的国际化功能，包括多语言支持、本地化、字符编码处理等

// 测试1: 多语言文本处理
test "多语言文本处理测试" {
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  
  // 配置支持的语言
  let supported_locales = ["zh-CN", "en-US", "ja-JP", "ko-KR", "fr-FR", "de-DE", "es-ES", "ru-RU"]
  for locale in supported_locales {
    i18n_manager.add_locale(locale)
  }
  
  // 添加多语言资源
  i18n_manager.add_translation("zh-CN", "span.name.default", "默认操作")
  i18n_manager.add_translation("en-US", "span.name.default", "Default Operation")
  i18n_manager.add_translation("ja-JP", "span.name.default", "デフォルト操作")
  i18n_manager.add_translation("ko-KR", "span.name.default", "기본 작업")
  i18n_manager.add_translation("fr-FR", "span.name.default", "Opération par défaut")
  i18n_manager.add_translation("de-DE", "span.name.default", "Standardoperation")
  i18n_manager.add_translation("es-ES", "span.name.default", "Operación predeterminada")
  i18n_manager.add_translation("ru-RU", "span.name.default", "Операция по умолчанию")
  
  i18n_manager.add_translation("zh-CN", "error.message.timeout", "操作超时")
  i18n_manager.add_translation("en-US", "error.message.timeout", "Operation timeout")
  i18n_manager.add_translation("ja-JP", "error.message.timeout", "操作タイムアウト")
  i18n_manager.add_translation("ko-KR", "error.message.timeout", "작업 시간 초과")
  
  // 测试多语言span名称
  let test_locales = ["zh-CN", "en-US", "ja-JP", "ko-KR"]
  let localized_spans = []
  
  for locale in test_locales {
    i18n_manager.set_current_locale(locale)
    let localized_name = i18n_manager.translate("span.name.default")
    
    let span = Span::new(localized_name, Server, TraceContext::new("i18n-trace", "i18n-span", true, ""))
    Span::set_attribute(span, "locale", StringValue(locale))
    Span::set_attribute(span, "localized.name", StringValue(localized_name))
    
    localized_spans = localized_spans.push(span)
  }
  
  // 验证多语言处理
  assert_eq(localized_spans.length(), test_locales.length())
  
  let chinese_span = localized_spans.find(fn(s) { 
    match Span::get_attribute(s, "locale") {
      Some(StringValue(locale)) => locale == "zh-CN"
      _ => false
    }
  })
  assert_true(chinese_span != None)
  
  match chinese_span {
    Some(span) => {
      assert_eq(span.name, "默认操作")
      let localized_name = Span::get_attribute(span, "localized.name")
      assert_eq(localized_name, Some(StringValue("默认操作")))
    }
    None => assert_true(false)
  }
  
  let english_span = localized_spans.find(fn(s) { 
    match Span::get_attribute(s, "locale") {
      Some(StringValue(locale)) => locale == "en-US"
      _ => false
    }
  })
  assert_true(english_span != None)
  
  match english_span {
    Some(span) => {
      assert_eq(span.name, "Default Operation")
      let localized_name = Span::get_attribute(span, "localized.name")
      assert_eq(localized_name, Some(StringValue("Default Operation")))
    }
    None => assert_true(false)
  }
  
  // 测试错误消息本地化
  for locale in ["zh-CN", "en-US", "ja-JP", "ko-KR"] {
    i18n_manager.set_current_locale(locale)
    let localized_error = i18n_manager.translate("error.message.timeout")
    
    if locale == "zh-CN" {
      assert_eq(localized_error, "操作超时")
    } else if locale == "en-US" {
      assert_eq(localized_error, "Operation timeout")
    } else if locale == "ja-JP" {
      assert_eq(localized_error, "操作タイムアウト")
    } else if locale == "ko-KR" {
      assert_eq(localized_error, "작업 시간 초과")
    }
  }
  
  // 测试回退机制
  i18n_manager.set_current_locale("pt-BR")  // 未配置的语言
  let fallback_text = i18n_manager.translate("span.name.default")
  assert_eq(fallback_text, "span.name.default")  // 应该返回原始键
}

// 测试2: 字符编码处理
test "字符编码处理测试" {
  // 创建编码处理器
  let encoding_handler = EncodingHandler::new()
  
  // 测试多种字符编码
  let test_strings = [
    { text: "中文测试", encoding: "UTF-8", language: "zh" },
    { text: "English Test", encoding: "UTF-8", language: "en" },
    { text: "日本語テスト", encoding: "UTF-8", language: "ja" },
    { text: "한국어 테스트", encoding: "UTF-8", language: "ko" },
    { text: "Тест на русском", encoding: "UTF-8", language: "ru" },
    { text: "Test en français", encoding: "UTF-8", language: "fr" },
    { text: "Test auf Deutsch", encoding: "UTF-8", language: "de" },
    { text: "Prueba en español", encoding: "UTF-8", language: "es" },
    { text: "اختبار باللغة العربية", encoding: "UTF-8", language: "ar" },
    { text: "עברית בדיקה", encoding: "UTF-8", language: "he" },
    { text: "हिंदी परीक्षण", encoding: "UTF-8", language: "hi" },
    { text: "ไทยการทดสอบ", encoding: "UTF-8", language: "th" }
  ]
  
  // 测试编码转换
  for test_string in test_strings {
    // 原始UTF-8编码
    let utf8_bytes = encoding_handler.encode(test_string.text, "UTF-8")
    assert_true(utf8_bytes.length() > 0)
    
    // 解码验证
    let decoded_text = encoding_handler.decode(utf8_bytes, "UTF-8")
    assert_eq(decoded_text, test_string.text)
    
    // 创建包含多语言文本的span
    let span = Span::new("encoding.test", Server, TraceContext::new("encoding-trace", "encoding-span", true, ""))
    Span::set_attribute(span, "multilingual.text", StringValue(test_string.text))
    Span::set_attribute(span, "language", StringValue(test_string.language))
    Span::set_attribute(span, "encoding", StringValue(test_string.encoding))
    
    // 序列化测试
    let serialized = JsonSerializer::serialize_span(span)
    assert_true(serialized.contains(test_string.text))
    
    // 反序列化测试
    let deserialized = JsonSerializer::deserialize_span(serialized)
    let multilingual_attr = Span::get_attribute(deserialized, "multilingual.text")
    assert_eq(multilingual_attr, Some(StringValue(test_string.text)))
  }
  
  // 测试特殊字符处理
  let special_characters = [
    "emoji: 😊😢🎉🚀",
    "math: ∑∏∫∆∇∂",
    "currency: $€£¥₹₽",
    "symbols: ©®™§¶†‡•…‰",
    "quotes: ""''""""「」『』【】",
    "punctuation: ，。；：？！""''（）【】《》",
    "arrows: ←→↑↓↖↗↘↙⇐⇒⇑⇓",
    "mixed: Hello 世界! 🌍 こんにちは test 123"
  ]
  
  for special_text in special_characters {
    // 测试特殊字符的编码处理
    let encoded = encoding_handler.encode(special_text, "UTF-8")
    let decoded = encoding_handler.decode(encoded, "UTF-8")
    assert_eq(decoded, special_text)
    
    // 测试在span属性中的使用
    let span = Span::new("special.char.test", Server, TraceContext::new("special-trace", "special-span", true, ""))
    Span::set_attribute(span, "special.text", StringValue(special_text))
    
    let attr_value = Span::get_attribute(span, "special.text")
    assert_eq(attr_value, Some(StringValue(special_text)))
  }
  
  // 测试编码错误处理
  let invalid_utf8 = [0xFF, 0xFE, 0xFD]  // 无效的UTF-8序列
  let error_handling_result = encoding_handler.safe_decode(invalid_utf8, "UTF-8")
  
  match error_handling_result {
    Ok(text) => assert_true(false)  // 不应该成功
    Err(error) => {
      assert_eq(error.error_type, "encoding_error")
      assert_true(error.message.contains("invalid UTF-8"))
    }
  }
  
  // 测试BOM处理
  let text_with_bom = "\uFEFF中文测试"  // 带BOM的文本
  let bom_encoded = encoding_handler.encode(text_with_bom, "UTF-8")
  let bom_removed = encoding_handler.remove_bom(bom_encoded)
  let bom_decoded = encoding_handler.decode(bom_removed, "UTF-8")
  assert_eq(bom_decoded, "中文测试")  // BOM应该被移除
}

// 测试3: 时区和日期时间本地化
test "时区和日期时间本地化测试" {
  // 创建时区管理器
  let timezone_manager = TimezoneManager::new()
  
  // 配置常用时区
  let timezones = [
    "UTC", "Asia/Shanghai", "Asia/Tokyo", "Asia/Seoul", "America/New_York", 
    "America/Los_Angeles", "Europe/London", "Europe/Paris", "Europe/Berlin",
    "Australia/Sydney", "Asia/Dubai", "Asia/Kolkata"
  ]
  
  for timezone in timezones {
    timezone_manager.add_timezone(timezone)
  }
  
  // 创建测试时间戳
  let test_timestamp = 1640995200  // 2022-01-01 00:00:00 UTC
  
  // 测试时区转换
  let timezone_results = []
  for timezone in timezones {
    let localized_time = timezone_manager.convert_timezone(test_timestamp, "UTC", timezone)
    let formatted_time = timezone_manager.format(localized_time, timezone, "YYYY-MM-DD HH:mm:ss Z")
    
    timezone_results = timezone_results.push({
      timezone: timezone,
      timestamp: localized_time,
      formatted: formatted_time
    })
  }
  
  // 验证时区转换结果
  let utc_result = timezone_results.find(fn(r) { r.timezone == "UTC" })
  assert_true(utc_result != None)
  match utc_result {
    Some(result) => {
      assert_eq(result.formatted, "2022-01-01 00:00:00 +0000")
    }
    None => assert_true(false)
  }
  
  let shanghai_result = timezone_results.find(fn(r) { r.timezone == "Asia/Shanghai" })
  assert_true(shanghai_result != None)
  match shanghai_result {
    Some(result) => {
      assert_eq(result.formatted, "2022-01-01 08:00:00 +0800")
    }
    None => assert_true(false)
  }
  
  let tokyo_result = timezone_results.find(fn(r) { r.timezone == "Asia/Tokyo" })
  assert_true(tokyo_result != None)
  match tokyo_result {
    Some(result) => {
      assert_eq(result.formatted, "2022-01-01 09:00:00 +0900")
    }
    None => assert_true(false)
  }
  
  let new_york_result = timezone_results.find(fn(r) { r.timezone == "America/New_York" })
  assert_true(new_york_result != None)
  match new_york_result {
    Some(result) => {
      assert_eq(result.formatted, "2021-12-31 19:00:00 -0500")
    }
    None => assert_true(false)
  }
  
  // 测试日期格式本地化
  let date_formatter = DateFormatter::new()
  
  let locale_formats = [
    { locale: "zh-CN", format: "YYYY年MM月DD日 HH:mm:ss", expected: "2022年01月01日 08:00:00" },
    { locale: "en-US", format: "MM/DD/YYYY hh:mm:ss A", expected: "01/01/2022 08:00:00 AM" },
    { locale: "ja-JP", format: "YYYY年MM月DD日 HH時mm分ss秒", expected: "2022年01月01日 08時00分00秒" },
    { locale: "ko-KR", format: "YYYY년 MM월 DD일 HH:mm:ss", expected: "2022년 01월 01일 08:00:00" },
    { locale: "de-DE", format: "DD.MM.YYYY HH:mm:ss", expected: "01.01.2022 08:00:00" },
    { locale: "fr-FR", format: "DD/MM/YYYY HH:mm:ss", expected: "01/01/2022 08:00:00" }
  ]
  
  for format_info in locale_formats {
    date_formatter.set_locale(format_info.locale)
    let localized_date = date_formatter.format(test_timestamp, "Asia/Shanghai", format_info.format)
    assert_eq(localized_date, format_info.expected)
  }
  
  // 测试时区感知的span创建
  let timezone_spans = []
  for timezone in ["UTC", "Asia/Shanghai", "America/New_York", "Europe/London"] {
    let span = Span::new("timezone.test", Server, TraceContext::new("timezone-trace", "timezone-span", true, ""))
    
    let local_time = timezone_manager.convert_timezone(test_timestamp, "UTC", timezone)
    let formatted_time = timezone_manager.format(local_time, timezone, "YYYY-MM-DD HH:mm:ss Z")
    
    Span::set_attribute(span, "timezone", StringValue(timezone))
    Span::set_attribute(span, "local.timestamp", StringValue(formatted_time))
    Span::set_attribute(span, "utc.timestamp", StringValue(timezone_manager.format(test_timestamp, "UTC", "YYYY-MM-DD HH:mm:ss Z")))
    
    timezone_spans = timezone_spans.push(span)
  }
  
  // 验证时区感知的span
  let shanghai_span = timezone_spans.find(fn(s) { 
    match Span::get_attribute(s, "timezone") {
      Some(StringValue(tz)) => tz == "Asia/Shanghai"
      _ => false
    }
  })
  assert_true(shanghai_span != None)
  
  match shanghai_span {
    Some(span) => {
      let local_time = Span::get_attribute(span, "local.timestamp")
      assert_eq(local_time, Some(StringValue("2022-01-01 08:00:00 +0800")))
      
      let utc_time = Span::get_attribute(span, "utc.timestamp")
      assert_eq(utc_time, Some(StringValue("2022-01-01 00:00:00 +0000")))
    }
    None => assert_true(false)
  }
}

// 测试4: 数字和货币格式本地化
test "数字和货币格式本地化测试" {
  // 创建数字格式化器
  let number_formatter = NumberFormatter::new()
  
  // 测试数字格式本地化
  let test_number = 1234567.89
  let expected_formats = [
    { locale: "zh-CN", expected: "1,234,567.89" },
    { locale: "en-US", expected: "1,234,567.89" },
    { locale: "de-DE", expected: "1.234.567,89" },
    { locale: "fr-FR", expected: "1 234 567,89" },
    { locale: "ja-JP", expected: "1,234,567.89" },
    { locale: "ko-KR", expected: "1,234,567.89" },
    { locale: "ru-RU", expected: "1 234 567,89" },
    { locale: "es-ES", expected: "1.234.567,89" }
  ]
  
  for format_info in expected_formats {
    number_formatter.set_locale(format_info.locale)
    let formatted_number = number_formatter.format(test_number)
    assert_eq(formatted_number, format_info.expected)
  }
  
  // 测试货币格式本地化
  let currency_formatter = CurrencyFormatter::new()
  let test_amount = 1234.56
  
  let currency_formats = [
    { locale: "zh-CN", currency: "CNY", expected: "¥1,234.56" },
    { locale: "en-US", currency: "USD", expected: "$1,234.56" },
    { locale: "de-DE", currency: "EUR", expected: "1.234,56 €" },
    { locale: "fr-FR", currency: "EUR", expected: "1 234,56 €" },
    { locale: "ja-JP", currency: "JPY", expected: "￥1,235" },
    { locale: "ko-KR", currency: "KRW", expected: "₩1,235" },
    { locale: "ru-RU", currency: "RUB", expected: "1 234,56 ₽" },
    { locale: "es-ES", currency: "EUR", expected: "1.234,56 €" }
  ]
  
  for currency_info in currency_formats {
    currency_formatter.set_locale(currency_info.locale)
    let formatted_currency = currency_formatter.format(test_amount, currency_info.currency)
    assert_eq(formatted_currency, currency_info.expected)
  }
  
  // 测试百分比格式本地化
  let percentage_formatter = PercentageFormatter::new()
  let test_percentage = 0.1234
  
  let percentage_formats = [
    { locale: "zh-CN", expected: "12.34%" },
    { locale: "en-US", expected: "12.34%" },
    { locale: "de-DE", expected: "12,34 %" },
    { locale: "fr-FR", expected: "12,34 %" },
    { locale: "ja-JP", expected: "12.34%" },
    { locale: "ko-KR", expected: "12.34%" },
    { locale: "ru-RU", expected: "12,34 %" },
    { locale: "es-ES", expected: "12,34 %" }
  ]
  
  for percentage_info in percentage_formats {
    percentage_formatter.set_locale(percentage_info.locale)
    let formatted_percentage = percentage_formatter.format(test_percentage)
    assert_eq(formatted_percentage, percentage_info.expected)
  }
  
  // 测试在span属性中的本地化数字
  let metric_spans = []
  for locale in ["zh-CN", "en-US", "de-DE", "ja-JP"] {
    let span = Span::new("metric.test", Server, TraceContext::new("metric-trace", "metric-span", true, ""))
    
    number_formatter.set_locale(locale)
    currency_formatter.set_locale(locale)
    percentage_formatter.set_locale(locale)
    
    Span::set_attribute(span, "locale", StringValue(locale))
    Span::set_attribute(span, "formatted.number", StringValue(number_formatter.format(test_number)))
    Span::set_attribute(span, "raw.number", FloatValue(test_number))
    
    let currency_code = if locale == "zh-CN" { "CNY" }
                       else if locale == "en-US" { "USD" }
                       else if locale == "de-DE" { "EUR" }
                       else { "JPY" }
    
    Span::set_attribute(span, "formatted.currency", StringValue(currency_formatter.format(test_amount, currency_code)))
    Span::set_attribute(span, "raw.amount", FloatValue(test_amount))
    
    Span::set_attribute(span, "formatted.percentage", StringValue(percentage_formatter.format(test_percentage)))
    Span::set_attribute(span, "raw.percentage", FloatValue(test_percentage))
    
    metric_spans = metric_spans.push(span)
  }
  
  // 验证本地化数字在span中的正确性
  let german_span = metric_spans.find(fn(s) { 
    match Span::get_attribute(s, "locale") {
      Some(StringValue(locale)) => locale == "de-DE"
      _ => false
    }
  })
  assert_true(german_span != None)
  
  match german_span {
    Some(span) => {
      let formatted_number = Span::get_attribute(span, "formatted.number")
      assert_eq(formatted_number, Some(StringValue("1.234.567,89")))
      
      let formatted_currency = Span::get_attribute(span, "formatted.currency")
      assert_eq(formatted_currency, Some(StringValue("1.234,56 €")))
      
      let formatted_percentage = Span::get_attribute(span, "formatted.percentage")
      assert_eq(formatted_percentage, Some(StringValue("12,34 %")))
    }
    None => assert_true(false)
  }
}

// 测试5: 文化特定的格式处理
test "文化特定的格式处理测试" {
  // 创建文化格式管理器
  let cultural_formatter = CulturalFormatter::new()
  
  // 测试姓名格式
  let name_test_cases = [
    { 
      locale: "zh-CN", 
      first_name: "小明", 
      last_name: "张", 
      expected_formats: [
        { style: "full", expected: "张小明" },
        { style: "formal", expected: "张先生" },
        { style: "informal", expected: "小明" }
      ]
    },
    { 
      locale: "en-US", 
      first_name: "John", 
      last_name: "Smith", 
      expected_formats: [
        { style: "full", expected: "John Smith" },
        { style: "formal", expected: "Mr. Smith" },
        { style: "informal", expected: "John" }
      ]
    },
    { 
      locale: "ja-JP", 
      first_name: "太郎", 
      last_name: "田中", 
      expected_formats: [
        { style: "full", expected: "田中太郎" },
        { style: "formal", expected: "田中さん" },
        { style: "informal", expected: "太郎" }
      ]
    },
    { 
      locale: "ko-KR", 
      first_name: "민준", 
      last_name: "김", 
      expected_formats: [
        { style: "full", expected: "김민준" },
        { style: "formal", expected: "김씨" },
        { style: "informal", expected: "민준" }
      ]
    }
  ]
  
  for test_case in name_test_cases {
    cultural_formatter.set_locale(test_case.locale)
    
    for format_info in test_case.expected_formats {
      let formatted_name = cultural_formatter.format_name(
        test_case.first_name, 
        test_case.last_name, 
        format_info.style
      )
      assert_eq(formatted_name, format_info.expected)
    }
  }
  
  // 测试地址格式
  let address_test_cases = [
    {
      locale: "zh-CN",
      components: {
        country: "中国",
        province: "北京市",
        city: "北京市",
        district: "朝阳区",
        street: "建国门外大街1号",
        postal_code: "100020"
      },
      expected: "中国北京市朝阳区建国门外大街1号 100020"
    },
    {
      locale: "en-US",
      components: {
        street: "123 Main Street",
        city: "New York",
        state: "NY",
        postal_code: "10001",
        country: "USA"
      },
      expected: "123 Main Street\nNew York, NY 10001\nUSA"
    },
    {
      locale: "ja-JP",
      components: {
        postal_code: "100-0001",
        prefecture: "東京都",
        city: "千代田区",
        ward: "千代田",
        street: "丸の内1-1-1"
      },
      expected: "〒100-0001 東京都千代田区丸の内1-1-1"
    }
  ]
  
  for test_case in address_test_cases {
    cultural_formatter.set_locale(test_case.locale)
    let formatted_address = cultural_formatter.format_address(test_case.components)
    assert_eq(formatted_address, test_case.expected)
  }
  
  // 测试电话号码格式
  let phone_test_cases = [
    { locale: "zh-CN", number: "+8613812345678", expected: "138-1234-5678" },
    { locale: "en-US", number: "+12125551234", expected: "(212) 555-1234" },
    { locale: "ja-JP", number: "+81312345678", expected: "03-1234-5678" },
    { locale: "ko-KR", number: "+8212345678", expected: "010-1234-5678" },
    { locale: "de-DE", number: "+4912345678", expected: "01234 5678" },
    { locale: "fr-FR", number: "+3312345678", expected: "01 23 45 67 89" }
  ]
  
  for test_case in phone_test_cases {
    cultural_formatter.set_locale(test_case.locale)
    let formatted_phone = cultural_formatter.format_phone_number(test_case.number)
    assert_eq(formatted_phone, test_case.expected)
  }
  
  // 测试度量单位格式
  let measurement_test_cases = [
    { 
      locale: "zh-CN", 
      value: 175.5, 
      unit: "height", 
      expected: "175.5厘米" 
    },
    { 
      locale: "en-US", 
      value: 175.5, 
      unit: "height", 
      expected: "5'9\"" 
    },
    { 
      locale: "zh-CN", 
      value: 70.5, 
      unit: "weight", 
      expected: "70.5公斤" 
    },
    { 
      locale: "en-US", 
      value: 70.5, 
      unit: "weight", 
      expected: "155.4 lbs" 
    },
    { 
      locale: "zh-CN", 
      value: 100.0, 
      unit: "temperature", 
      expected: "100.0°C" 
    },
    { 
      locale: "en-US", 
      value: 100.0, 
      unit: "temperature", 
      expected: "212.0°F" 
    }
  ]
  
  for test_case in measurement_test_cases {
    cultural_formatter.set_locale(test_case.locale)
    let formatted_measurement = cultural_formatter.format_measurement(test_case.value, test_case.unit)
    assert_eq(formatted_measurement, test_case.expected)
  }
  
  // 测试文化特定的span属性
  let cultural_spans = []
  for locale in ["zh-CN", "en-US", "ja-JP"] {
    let span = Span::new("cultural.test", Server, TraceContext::new("cultural-trace", "cultural-span", true, ""))
    
    cultural_formatter.set_locale(locale)
    
    if locale == "zh-CN" {
      Span::set_attribute(span, "user.name", StringValue(cultural_formatter.format_name("小明", "张", "full")))
      Span::set_attribute(span, "user.address", StringValue(cultural_formatter.format_address({
        country: "中国",
        province: "北京市",
        city: "北京市",
        district: "朝阳区",
        street: "建国门外大街1号",
        postal_code: "100020"
      })))
      Span::set_attribute(span, "user.phone", StringValue(cultural_formatter.format_phone_number("+8613812345678")))
      Span::set_attribute(span, "user.height", StringValue(cultural_formatter.format_measurement(175.5, "height")))
    } else if locale == "en-US" {
      Span::set_attribute(span, "user.name", StringValue(cultural_formatter.format_name("John", "Smith", "full")))
      Span::set_attribute(span, "user.address", StringValue(cultural_formatter.format_address({
        street: "123 Main Street",
        city: "New York",
        state: "NY",
        postal_code: "10001",
        country: "USA"
      })))
      Span::set_attribute(span, "user.phone", StringValue(cultural_formatter.format_phone_number("+12125551234")))
      Span::set_attribute(span, "user.height", StringValue(cultural_formatter.format_measurement(175.5, "height")))
    } else if locale == "ja-JP" {
      Span::set_attribute(span, "user.name", StringValue(cultural_formatter.format_name("太郎", "田中", "full")))
      Span::set_attribute(span, "user.address", StringValue(cultural_formatter.format_address({
        postal_code: "100-0001",
        prefecture: "東京都",
        city: "千代田区",
        ward: "千代田",
        street: "丸の内1-1-1"
      })))
      Span::set_attribute(span, "user.phone", StringValue(cultural_formatter.format_phone_number("+81312345678")))
      Span::set_attribute(span, "user.height", StringValue(cultural_formatter.format_measurement(175.5, "height")))
    }
    
    Span::set_attribute(span, "locale", StringValue(locale))
    cultural_spans = cultural_spans.push(span)
  }
  
  // 验证文化特定的span属性
  let chinese_span = cultural_spans.find(fn(s) { 
    match Span::get_attribute(s, "locale") {
      Some(StringValue(locale)) => locale == "zh-CN"
      _ => false
    }
  })
  assert_true(chinese_span != None)
  
  match chinese_span {
    Some(span) => {
      let user_name = Span::get_attribute(span, "user.name")
      assert_eq(user_name, Some(StringValue("张小明")))
      
      let user_phone = Span::get_attribute(span, "user.phone")
      assert_eq(user_phone, Some(StringValue("138-1234-5678")))
      
      let user_height = Span::get_attribute(span, "user.height")
      assert_eq(user_height, Some(StringValue("175.5厘米")))
    }
    None => assert_true(false)
  }
}

// 测试6: 右到左语言支持
test "右到左语言支持测试" {
  // 创建双向文本处理器
  let bidi_processor = BidiProcessor::new()
  
  // 测试RTL语言
  let rtl_test_cases = [
    { 
      language: "ar", 
      text: "مرحبا بالعالم", 
      direction: "rtl",
      expected_visual: "مرحبا بالعالم"
    },
    { 
      language: "he", 
      text: "שלום עולם", 
      direction: "rtl",
      expected_visual: "שלום עולם"
    },
    { 
      language: "fa", 
      text: "سلام دنیا", 
      direction: "rtl",
      expected_visual: "سلام دنیا"
    },
    { 
      language: "ur", 
      text: "ہیلو دنیا", 
      direction: "rtl",
      expected_visual: "ہیلو دنیا"
    }
  ]
  
  for test_case in rtl_test_cases {
    bidi_processor.set_direction(test_case.direction)
    
    // 检测文本方向
    let detected_direction = bidi_processor.detect_direction(test_case.text)
    assert_eq(detected_direction, test_case.direction)
    
    // 处理双向文本
    let processed_text = bidi_processor.process(test_case.text)
    assert_eq(processed_text, test_case.expected_visual)
    
    // 创建包含RTL文本的span
    let span = Span::new("rtl.test", Server, TraceContext::new("rtl-trace", "rtl-span", true, ""))
    Span::set_attribute(span, "language", StringValue(test_case.language))
    Span::set_attribute(span, "text.direction", StringValue(test_case.direction))
    Span::set_attribute(span, "rtl.text", StringValue(test_case.text))
    Span::set_attribute(span, "processed.text", StringValue(processed_text))
    
    // 验证RTL文本在span中的处理
    let rtl_attr = Span::get_attribute(span, "rtl.text")
    assert_eq(rtl_attr, Some(StringValue(test_case.text)))
    
    let processed_attr = Span::get_attribute(span, "processed.text")
    assert_eq(processed_attr, Some(StringValue(processed_text)))
  }
  
  // 测试混合LTR和RTL文本
  let mixed_text_cases = [
    { 
      text: "Hello مرحبا World", 
      expected: "Hello مرحبا World",
      base_direction: "ltr"
    },
    { 
      text: "مرحبا Hello العالم", 
      expected: "مرحبا Hello العالم",
      base_direction: "rtl"
    },
    { 
      text: "123 مرحبا 456", 
      expected: "123 مرحبا 456",
      base_direction: "ltr"
    },
    { 
      text: "مرحبا (123) test", 
      expected: "مرحبا (123) test",
      base_direction: "rtl"
    }
  ]
  
  for mixed_case in mixed_text_cases {
    bidi_processor.set_base_direction(mixed_case.base_direction)
    let processed_mixed = bidi_processor.process(mixed_case.text)
    assert_eq(processed_mixed, mixed_case.expected)
  }
  
  // 测试RTL数字和标点符号处理
  let rtl_numbers_test = [
    { text: "١٢٣٤٥٦٧٨٩", expected: "١٢٣٤٥٦٧٨٩" },  // 阿拉伯数字
    { text: "۱۲۳۴۵۶۷۸۹", expected: "۱۲۳۴۵۶۷۸۹" },  // 波斯数字
    { text: "مرحبا! كيف حالك؟", expected: "مرحبا! كيف حالك؟" },  // RTL标点
    { text: "(مرحبا) [test]", expected: "(مرحبا) [test]" }  // 混合括号
  ]
  
  for number_test in rtl_numbers_test {
    let processed_numbers = bidi_processor.process(number_test.text)
    assert_eq(processed_numbers, number_test.expected)
  }
  
  // 测试RTL文本的序列化和反序列化
  let rtl_span = Span::new("rtl.serialization.test", Server, TraceContext::new("rtl-serialization-trace", "rtl-serialization-span", true, ""))
  Span::set_attribute(span, "arabic.text", StringValue("مرحبا بالعالم"))
  Span::set_attribute(span, "hebrew.text", StringValue("שלום עולם"))
  Span::set_attribute(span, "mixed.text", StringValue("Hello مرحبا World"))
  
  // 序列化
  let serialized = JsonSerializer::serialize_span(rtl_span)
  assert_true(serialized.contains("مرحبا بالعالم"))
  assert_true(serialized.contains("שלום עולם"))
  assert_true(serialized.contains("Hello مرحبا World"))
  
  // 反序列化
  let deserialized = JsonSerializer::deserialize_span(serialized)
  let arabic_attr = Span::get_attribute(deserialized, "arabic.text")
  let hebrew_attr = Span::get_attribute(deserialized, "hebrew.text")
  let mixed_attr = Span::get_attribute(deserialized, "mixed.text")
  
  assert_eq(arabic_attr, Some(StringValue("مرحبا بالعالم")))
  assert_eq(hebrew_attr, Some(StringValue("שלום עולם")))
  assert_eq(mixed_attr, Some(StringValue("Hello مرحبا World")))
}

// 测试7: 复合脚本和多语言混合
test "复合脚本和多语言混合测试" {
  // 创建复合脚本处理器
  let complex_script_processor = ComplexScriptProcessor::new()
  
  // 测试复合脚本语言
  let complex_script_cases = [
    { 
      language: "hi", 
      text: "नमस्ते दुनिया", 
      script: "Devanagari"
    },
    { 
      language: "th", 
      text: "สวัสดีชาวโลก", 
      script: "Thai"
    },
    { 
      language: "ko", 
      text: "안녕하세요 세계", 
      script: "Hangul"
    },
    { 
      language: "zh", 
      text: "你好世界", 
      script: "Han"
    },
    { 
      language: "ja", 
      text: "こんにちは世界", 
      script: "Han+Kana"
    }
  ]
  
  for test_case in complex_script_cases {
    // 检测脚本
    let detected_script = complex_script_processor.detect_script(test_case.text)
    assert_eq(detected_script, test_case.script)
    
    // 处理复合脚本
    let processed_text = complex_script_processor.process(test_case.text)
    assert_eq(processed_text, test_case.text)
    
    // 创建包含复合脚本的span
    let span = Span::new("complex.script.test", Server, TraceContext::new("complex-script-trace", "complex-script-span", true, ""))
    Span::set_attribute(span, "language", StringValue(test_case.language))
    Span::set_attribute(span, "script", StringValue(test_case.script))
    Span::set_attribute(span, "complex.text", StringValue(test_case.text))
    
    // 验证复合脚本在span中的处理
    let script_attr = Span::get_attribute(span, "script")
    assert_eq(script_attr, Some(StringValue(test_case.script)))
    
    let text_attr = Span::get_attribute(span, "complex.text")
    assert_eq(text_attr, Some(StringValue(test_case.text)))
  }
  
  // 测试多语言混合文本
  let multilingual_mixed_cases = [
    { 
      text: "Hello 你好 こんにちは 안녕하세요", 
      languages: ["en", "zh", "ja", "ko"],
      expected_segments: [
        { text: "Hello", language: "en" },
        { text: "你好", language: "zh" },
        { text: "こんにちは", language: "ja" },
        { text: "안녕하세요", language: "ko" }
      ]
    },
    { 
      text: "مرحبا Hello שלום", 
      languages: ["ar", "en", "he"],
      expected_segments: [
        { text: "مرحبا", language: "ar" },
        { text: "Hello", language: "en" },
        { text: "שלום", language: "he" }
      ]
    },
    { 
      text: "नमस्ते こんにちは สวัสดี", 
      languages: ["hi", "ja", "th"],
      expected_segments: [
        { text: "नमस्ते", language: "hi" },
        { text: "こんにちは", language: "ja" },
        { text: "สวัสดี", language: "th" }
      ]
    }
  ]
  
  for mixed_case in multilingual_mixed_cases {
    // 分段多语言文本
    let segments = complex_script_processor.segment_multilingual(mixed_case.text)
    assert_eq(segments.length(), mixed_case.expected_segments.length())
    
    for i in 0..=segments.length() - 1 {
      assert_eq(segments[i].text, mixed_case.expected_segments[i].text)
      assert_eq(segments[i].language, mixed_case.expected_segments[i].language)
    }
    
    // 创建包含多语言混合的span
    let span = Span::new("multilingual.mixed.test", Server, TraceContext::new("multilingual-mixed-trace", "multilingual-mixed-span", true, ""))
    Span::set_attribute(span, "mixed.text", StringValue(mixed_case.text))
    Span::set_attribute(span, "language.count", IntValue(mixed_case.languages.length()))
    
    // 添加语言分段信息
    for segment in segments {
      Span::set_attribute(span, "segment." + segment.language, StringValue(segment.text))
    }
    
    // 验证多语言混合在span中的处理
    let mixed_attr = Span::get_attribute(span, "mixed.text")
    assert_eq(mixed_attr, Some(StringValue(mixed_case.text)))
    
    let lang_count_attr = Span::get_attribute(span, "language.count")
    assert_eq(lang_count_attr, Some(IntValue(mixed_case.languages.length())))
  }
  
  // 测试音译和转写
  let transliteration_test_cases = [
    { 
      from: "zh", 
      to: "en", 
      text: "你好", 
      expected: "ni hao"
    },
    { 
      from: "ja", 
      to: "en", 
      text: "こんにちは", 
      expected: "konnichiwa"
    },
    { 
      from: "ru", 
      to: "en", 
      text: "Привет", 
      expected: "Privet"
    },
    { 
      from: "ar", 
      to: "en", 
      text: "مرحبا", 
      expected: "marhaban"
    }
  ]
  
  for transliteration_case in transliteration_test_cases {
    let transliterated = complex_script_processor.transliterate(
      transliteration_case.text, 
      transliteration_case.from, 
      transliteration_case.to
    )
    assert_eq(transliterated, transliteration_case.expected)
    
    // 创建包含音译的span
    let span = Span::new("transliteration.test", Server, TraceContext::new("transliteration-trace", "transliteration-span", true, ""))
    Span::set_attribute(span, "original.language", StringValue(transliteration_case.from))
    Span::set_attribute(span, "target.language", StringValue(transliteration_case.to))
    Span::set_attribute(span, "original.text", StringValue(transliteration_case.text))
    Span::set_attribute(span, "transliterated.text", StringValue(transliterated))
    
    // 验证音译在span中的处理
    let original_attr = Span::get_attribute(span, "original.text")
    assert_eq(original_attr, Some(StringValue(transliteration_case.text)))
    
    let transliterated_attr = Span::get_attribute(span, "transliterated.text")
    assert_eq(transliterated_attr, Some(StringValue(transliterated)))
  }
}

// 测试8: 国际化配置和回退机制
test "国际化配置和回退机制测试" {
  // 创建国际化配置管理器
  let i18n_config_manager = I18nConfigManager::new()
  
  // 配置默认语言和回退语言
  i18n_config_manager.set_default_locale("en-US")
  i18n_config_manager.set_fallback_locale("en-US")
  i18n_config_manager.set_preferred_locales(["zh-CN", "ja-JP", "ko-KR", "en-US"])
  
  // 添加翻译资源
  i18n_config_manager.add_translation_bundle("zh-CN", {
    "span.name.default": "默认操作",
    "error.message.timeout": "操作超时",
    "status.success": "成功",
    "status.failed": "失败"
  })
  
  i18n_config_manager.add_translation_bundle("ja-JP", {
    "span.name.default": "デフォルト操作",
    "error.message.timeout": "操作タイムアウト",
    "status.success": "成功"
    // 故意缺少 "status.failed" 翻译
  })
  
  i18n_config_manager.add_translation_bundle("en-US", {
    "span.name.default": "Default Operation",
    "error.message.timeout": "Operation timeout",
    "status.success": "Success",
    "status.failed": "Failed"
  })
  
  // 测试语言协商
  let negotiation_test_cases = [
    { 
      accepted_locales: ["zh-CN", "en-US"], 
      expected: "zh-CN" 
    },
    { 
      accepted_locales: ["ja-JP", "ko-KR"], 
      expected: "ja-JP" 
    },
    { 
      accepted_locales: ["fr-FR", "de-DE"], 
      expected: "en-US"  // 回退到默认语言
    },
    { 
      accepted_locales: ["ko-KR"], 
      expected: "en-US"  // ko-KR未配置，回退到默认语言
    }
  ]
  
  for negotiation_case in negotiation_test_cases {
    let negotiated_locale = i18n_config_manager.negotiate_locale(negotiation_case.accepted_locales)
    assert_eq(negotiated_locale, negotiation_case.expected)
  }
  
  // 测试翻译回退机制
  let fallback_test_cases = [
    { 
      locale: "zh-CN", 
      key: "span.name.default", 
      expected: "默认操作",
      fallback_used: false
    },
    { 
      locale: "ja-JP", 
      key: "status.failed", 
      expected: "Failed",
      fallback_used: true  // ja-JP缺少此翻译，回退到en-US
    },
    { 
      locale: "fr-FR", 
      key: "error.message.timeout", 
      expected: "Operation timeout",
      fallback_used: true  // fr-FR未配置，回退到en-US
    },
    { 
      locale: "ko-KR", 
      key: "nonexistent.key", 
      expected: "nonexistent.key",
      fallback_used: true  // 键不存在，返回原始键
    }
  ]
  
  for fallback_case in fallback_test_cases {
    let translation_result = i18n_config_manager.translate_with_fallback(
      fallback_case.locale, 
      fallback_case.key
    )
    
    assert_eq(translation_result.text, fallback_case.expected)
    assert_eq(translation_result.fallback_used, fallback_case.fallback_used)
    
    if fallback_case.fallback_used {
      assert_true(translation_result.fallback_locale.length() > 0)
    }
  }
  
  // 测试动态语言切换
  let dynamic_test_spans = []
  
  // 初始使用中文
  i18n_config_manager.set_current_locale("zh-CN")
  let zh_name = i18n_config_manager.translate("span.name.default")
  let zh_span = Span::new(zh_name, Server, TraceContext::new("dynamic-trace", "dynamic-span-1", true, ""))
  Span::set_attribute(span, "locale", StringValue("zh-CN"))
  dynamic_test_spans = dynamic_test_spans.push(zh_span)
  
  // 切换到日语
  i18n_config_manager.set_current_locale("ja-JP")
  let ja_name = i18n_config_manager.translate("span.name.default")
  let ja_span = Span::new(ja_name, Server, TraceContext::new("dynamic-trace", "dynamic-span-2", true, ""))
  Span::set_attribute(span, "locale", StringValue("ja-JP"))
  dynamic_test_spans = dynamic_test_spans.push(ja_span)
  
  // 切换到英语
  i18n_config_manager.set_current_locale("en-US")
  let en_name = i18n_config_manager.translate("span.name.default")
  let en_span = Span::new(en_name, Server, TraceContext::new("dynamic-trace", "dynamic-span-3", true, ""))
  Span::set_attribute(span, "locale", StringValue("en-US"))
  dynamic_test_spans = dynamic_test_spans.push(en_span)
  
  // 验证动态语言切换
  assert_eq(dynamic_test_spans.length(), 3)
  assert_eq(dynamic_test_spans[0].name, "默认操作")
  assert_eq(dynamic_test_spans[1].name, "デフォルト操作")
  assert_eq(dynamic_test_spans[2].name, "Default Operation")
  
  // 测试配置热更新
  let hot_update_test = fn() {
    // 添加新的翻译
    i18n_config_manager.add_translation("zh-CN", "new.key", "新的键")
    
    // 验证新翻译立即可用
    let new_translation = i18n_config_manager.translate("new.key")
    assert_eq(new_translation, "新的键")
    
    // 更新现有翻译
    i18n_config_manager.update_translation("zh-CN", "span.name.default", "更新的默认操作")
    
    // 验证更新生效
    let updated_translation = i18n_config_manager.translate("span.name.default")
    assert_eq(updated_translation, "更新的默认操作")
  }
  
  hot_update_test()
  
  // 测试配置持久化
  let config_serialized = i18n_config_manager.serialize_config()
  assert_true(config_serialized.length() > 0)
  
  let new_config_manager = I18nConfigManager::new()
  new_config_manager.deserialize_config(config_serialized)
  
  // 验证配置恢复
  let restored_translation = new_config_manager.translate("span.name.default")
  assert_eq(restored_translation, "更新的默认操作")
  
  let restored_default = new_config_manager.get_default_locale()
  assert_eq(restored_default, "en-US")
}

// 测试9: 无障碍和本地化质量
test "无障碍和本地化质量测试" {
  // 创建无障碍和本地化质量管理器
  let accessibility_i18n_manager = AccessibilityI18nManager::new()
  
  // 测试文本方向无障碍
  let accessibility_direction_tests = [
    { 
      text: "Hello World", 
      expected_direction: "ltr",
      accessibility_attributes: ["dir=\"ltr\""]
    },
    { 
      text: "مرحبا بالعالم", 
      expected_direction: "rtl",
      accessibility_attributes: ["dir=\"rtl\""]
    },
    { 
      text: "Hello مرحبا World", 
      expected_direction: "auto",
      accessibility_attributes: ["dir=\"auto\""]
    }
  ]
  
  for test_case in accessibility_direction_tests {
    let direction_result = accessibility_i18n_manager.determine_direction(test_case.text)
    assert_eq(direction_result.direction, test_case.expected_direction)
    
    let accessibility_attrs = accessibility_i18n_manager.generate_accessibility_attributes(test_case.text)
    for attr in test_case.accessibility_attributes {
      assert_true(accessibility_attrs.contains(attr))
    }
  }
  
  // 测试语言标签无障碍
  let accessibility_language_tests = [
    { 
      text: "Hello World", 
      expected_lang: "en",
      accessibility_attributes: ["lang=\"en\""]
    },
    { 
      text: "مرحبا بالعالم", 
      expected_lang: "ar",
      accessibility_attributes: ["lang=\"ar\""]
    },
    { 
      text: "你好世界", 
      expected_lang: "zh",
      accessibility_attributes: ["lang=\"zh\""]
    }
  ]
  
  for test_case in accessibility_language_tests {
    let language_result = accessibility_i18n_manager.detect_language(test_case.text)
    assert_eq(language_result.language, test_case.expected_lang)
    
    let accessibility_attrs = accessibility_i18n_manager.generate_accessibility_attributes(test_case.text)
    for attr in test_case.accessibility_attributes {
      assert_true(accessibility_attrs.contains(attr))
    }
  }
  
  // 测试本地化质量评估
  let quality_assessor = LocalizationQualityAssessor::new()
  
  let quality_test_cases = [
    {
      locale: "zh-CN",
      translations: [
        { key: "span.name.default", translation: "默认操作" },
        { key: "error.message.timeout", translation: "操作超时" },
        { key: "status.success", translation: "成功" }
      ],
      expected_quality_score: 0.9
    },
    {
      locale: "ja-JP",
      translations: [
        { key: "span.name.default", translation: "デフォルト操作" },
        { key: "error.message.timeout", translation: "操作タイムアウト" },
        { key: "status.success", translation: "成功" }
      ],
      expected_quality_score: 0.85
    },
    {
      locale: "ko-KR",
      translations: [
        { key: "span.name.default", translation: "기본 작업" },
        { key: "error.message.timeout", translation: "작업 시간 초과" },
        { key: "status.success", translation: "성공" }
      ],
      expected_quality_score: 0.8
    }
  ]
  
  for quality_case in quality_test_cases {
    let quality_result = quality_assessor.assess_quality(quality_case.locale, quality_case.translations)
    assert_true(quality_result.overall_score >= quality_case.expected_quality_score)
    
    // 验证质量指标
    assert_true(quality_result.completeness_score > 0.8)  // 完整性
    assert_true(quality_result.consistency_score > 0.7)   // 一致性
    assert_true(quality_result.accuracy_score > 0.8)     // 准确性
  }
  
  // 测试无障碍span创建
  let accessibility_spans = []
  let accessibility_test_texts = [
    { text: "Hello World", locale: "en-US" },
    { text: "مرحبا بالعالم", locale: "ar-SA" },
    { text: "你好世界", locale: "zh-CN" },
    { text: "こんにちは世界", locale: "ja-JP" }
  ]
  
  for test_text in accessibility_test_texts {
    let span = accessibility_i18n_manager.create_accessible_span(
      "accessibility.test", 
      Server, 
      TraceContext::new("accessibility-trace", "accessibility-span", true, ""),
      test_text.text,
      test_text.locale
    )
    
    // 验证无障碍属性
    let accessibility_attrs = Span::get_attribute(span, "accessibility.attributes")
    assert_true(accessibility_attrs != None)
    
    match accessibility_attrs {
      Some(StringValue(attrs)) => {
        assert_true(attrs.contains("lang=") or attrs.contains("dir="))
      }
      _ => assert_true(false)
    }
    
    accessibility_spans = accessibility_spans.push(span)
  }
  
  // 验证无障碍span
  assert_eq(accessibility_spans.length(), accessibility_test_texts.length())
  
  // 测试屏幕阅读器友好格式
  let screen_reader_tests = [
    { 
      text: "Error: Operation timeout", 
      locale: "en-US",
      expected_screen_reader: "Error: Operation timeout"
    },
    { 
      text: "错误：操作超时", 
      locale: "zh-CN",
      expected_screen_reader: "错误，操作超时"
    },
    { 
      text: "エラー：操作タイムアウト", 
      locale: "ja-JP",
      expected_screen_reader: "エラー、操作タイムアウト"
    }
  ]
  
  for screen_reader_test in screen_reader_tests {
    let screen_reader_format = accessibility_i18n_manager.format_for_screen_reader(
      screen_reader_test.text, 
      screen_reader_test.locale
    )
    assert_eq(screen_reader_format, screen_reader_test.expected_screen_reader)
  }
  
  // 测试高对比度模式支持
  let high_contrast_tests = [
    { 
      status: "success", 
      expected_high_contrast: "✓ 成功",
      expected_color: "#00FF00"
    },
    { 
      status: "error", 
      expected_high_contrast: "✗ 错误",
      expected_color: "#FF0000"
    },
    { 
      status: "warning", 
      expected_high_contrast: "⚠ 警告",
      expected_color: "#FFFF00"
    }
  ]
  
  for high_contrast_test in high_contrast_tests {
    let high_contrast_format = accessibility_i18n_manager.format_for_high_contrast(
      high_contrast_test.status, 
      "zh-CN"
    )
    assert_eq(high_contrast_format.text, high_contrast_test.expected_high_contrast)
    assert_eq(high_contrast_format.color, high_contrast_test.expected_color)
  }
}

// 测试10: 性能和资源优化
test "性能和资源优化测试" {
  // 创建国际化性能优化管理器
  let i18n_performance_manager = I18nPerformanceManager::new()
  
  // 配置性能优化策略
  i18n_performance_manager.enable_lazy_loading(true)
  i18n_performance_manager.enable_translation_caching(true)
  i18n_performance_manager.set_cache_size(1000)
  i18n_performance_manager.enable_preloading(true)
  
  // 添加大量翻译资源
  let large_translation_set = []
  for i in 0..=10000 {
    large_translation_set = large_translation_set.push({
      key: "test.key." + i.to_string(),
      translations: [
        { locale: "zh-CN", text: "测试文本 " + i.to_string() },
        { locale: "en-US", text: "Test text " + i.to_string() },
        { locale: "ja-JP", text: "テストテキスト " + i.to_string() },
        { locale: "ko-KR", text: "테스트 텍스트 " + i.to_string() }
      ]
    })
  }
  
  // 测试懒加载性能
  let lazy_load_start = Time::now()
  i18n_performance_manager.add_translation_set(large_translation_set)
  let lazy_load_end = Time::now()
  let lazy_load_duration = lazy_load_end - lazy_load_start
  
  // 验证懒加载不会阻塞初始化
  assert_true(lazy_load_duration < 100)  // 应该在100ms内完成
  
  // 测试翻译缓存性能
  let cache_test_start = Time::now()
  
  // 第一次访问（缓存未命中）
  let first_translation = i18n_performance_manager.translate("test.key.100", "zh-CN")
  
  let cache_test_mid = Time::now()
  
  // 第二次访问（缓存命中）
  let second_translation = i18n_performance_manager.translate("test.key.100", "zh-CN")
  
  let cache_test_end = Time::now()
  
  let first_access_duration = cache_test_mid - cache_test_start
  let second_access_duration = cache_test_end - cache_test_mid
  
  // 验证缓存效果
  assert_eq(first_translation, second_translation)
  assert_true(second_access_duration < first_access_duration)  // 缓存访问应该更快
  
  // 测试批量翻译性能
  let batch_keys = []
  for i in 0..=100 {
    batch_keys = batch_keys.push("test.key." + i.to_string())
  }
  
  let batch_start = Time::now()
  let batch_translations = i18n_performance_manager.translate_batch(batch_keys, "zh-CN")
  let batch_end = Time::now()
  let batch_duration = batch_end - batch_start
  
  // 验证批量翻译性能
  assert_eq(batch_translations.length(), batch_keys.length())
  assert_true(batch_duration < 50)  // 批量翻译应该在50ms内完成
  
  // 测试预加载性能
  let preload_locales = ["zh-CN", "en-US", "ja-JP", "ko-KR"]
  let preload_keys = ["test.key.1", "test.key.2", "test.key.3"]
  
  let preload_start = Time::now()
  i18n_performance_manager.preload_translations(preload_locales, preload_keys)
  let preload_end = Time::now()
  let preload_duration = preload_end - preload_start
  
  // 验证预加载性能
  assert_true(preload_duration < 200)  // 预加载应该在200ms内完成
  
  // 验证预加载效果
  for locale in preload_locales {
    for key in preload_keys {
      let preloaded_translation = i18n_performance_manager.translate(key, locale)
      assert_true(preloaded_translation.length() > 0)
    }
  }
  
  // 测试内存使用优化
  let initial_memory = MemoryManager::get_usage()
  
  // 创建大量国际化span
  let i18n_spans = []
  for i in 0..=1000 {
    let span = Span::new("i18n.performance.test", Server, 
                        TraceContext::new("i18n-perf-trace", "i18n-perf-span-" + i.to_string(), true, ""))
    
    // 添加多语言属性
    Span::set_attribute(span, "zh-CN.name", StringValue(i18n_performance_manager.translate("test.key." + i.to_string(), "zh-CN")))
    Span::set_attribute(span, "en-US.name", StringValue(i18n_performance_manager.translate("test.key." + i.to_string(), "en-US")))
    Span::set_attribute(span, "ja-JP.name", StringValue(i18n_performance_manager.translate("test.key." + i.to_string(), "ja-JP")))
    
    i18n_spans = i18n_spans.push(span)
  }
  
  let peak_memory = MemoryManager::get_usage()
  let memory_increase = peak_memory - initial_memory
  let memory_per_span = memory_increase.to_float() / i18n_spans.length().to_float()
  
  // 验证内存使用优化
  assert_true(memory_per_span < 2048)  // 每个span内存使用应该小于2KB
  
  // 测试缓存清理
  i18n_performance_manager.clear_cache()
  let cache_cleared_memory = MemoryManager::get_usage()
  let memory_reduced = peak_memory - cache_cleared_memory
  
  // 验证缓存清理效果
  assert_true(memory_reduced > 0)  // 清理缓存应该释放内存
  
  // 测试并发性能
  let thread_pool = ThreadPool::new(10)
  let concurrent_operations = []
  
  for i in 0..=100 {
    let future = thread_pool.spawn(fn() {
      let locale = ["zh-CN", "en-US", "ja-JP", "ko-KR"][i % 4]
      let key = "test.key." + (i % 1000).to_string()
      i18n_performance_manager.translate(key, locale)
    })
    concurrent_operations = concurrent_operations.push(future)
  }
  
  let concurrent_start = Time::now()
  
  // 等待所有并发操作完成
  let concurrent_results = []
  for future in concurrent_operations {
    let result = future.wait()
    concurrent_results = concurrent_results.push(result)
  }
  
  let concurrent_end = Time::now()
  let concurrent_duration = concurrent_end - concurrent_start
  let concurrent_throughput = concurrent_results.length().to_float() / concurrent_duration.to_float() * 1000.0
  
  // 验证并发性能
  assert_eq(concurrent_results.length(), 101)
  assert_true(concurrent_throughput > 1000)  // 至少1000次翻译/秒
  
  // 测试资源压缩
  let resource_compression_start = Time::now()
  let compressed_resources = i18n_performance_manager.compress_resources()
  let resource_compression_end = Time::now()
  let resource_compression_duration = resource_compression_end - resource_compression_start
  
  // 验证资源压缩
  assert_true(compressed_resources.compressed_size < compressed_resources.original_size)
  let compression_ratio = 1.0 - (compressed_resources.compressed_size.to_float() / compressed_resources.original_size.to_float())
  assert_true(compression_ratio > 0.3)  // 至少30%压缩率
  assert_true(resource_compression_duration < 1000)  // 压缩应该在1秒内完成
}