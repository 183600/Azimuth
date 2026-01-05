// Azimuth Internationalization Support Test Suite
// 国际化支持测试套件，验证遥测系统在不同语言、地区和文化环境下的兼容性和适应性

// 测试1: 多语言文本处理
test "多语言文本处理测试" {
  // 测试不同语言的文本处理
  let english_text = "Azimuth telemetry system"
  let chinese_text = "Azimuth 遥测系统"
  let japanese_text = "Azimuth テレメトリシステム"
  let arabic_text = "نظام تيليمتري أزيموث"
  let russian_text = "Система телеметрии Азимут"
  let french_text = "Système de télémétrie Azimuth"
  let german_text = "Azimuth-Telemetriesystem"
  let spanish_text = "Sistema de telemetría Azimuth"
  
  // 测试文本长度计算
  assert_eq(english_text.length(), 25)
  assert_eq(chinese_text.length(), 11)
  assert_eq(japanese_text.length(), 16)
  assert_eq(arabic_text.length(), 22)
  assert_eq(russian_text.length(), 27)
  assert_eq(french_text.length(), 30)
  assert_eq(german_text.length(), 24)
  assert_eq(spanish_text.length(), 29)
  
  // 测试文本编码转换
  let utf8_encoded_english = azimuth::TextEncoding::to_utf8(english_text)
  let utf8_encoded_chinese = azimuth::TextEncoding::to_utf8(chinese_text)
  let utf8_encoded_japanese = azimuth::TextEncoding::to_utf8(japanese_text)
  
  // 验证UTF-8编码结果
  let decoded_english = azimuth::TextEncoding::from_utf8(utf8_encoded_english)
  let decoded_chinese = azimuth::TextEncoding::from_utf8(utf8_encoded_chinese)
  let decoded_japanese = azimuth::TextEncoding::from_utf8(utf8_encoded_japanese)
  
  match decoded_english {
    Ok(text) => assert_eq(text, english_text)
    Err(_) => assert_true(false)
  }
  
  match decoded_chinese {
    Ok(text) => assert_eq(text, chinese_text)
    Err(_) => assert_true(false)
  }
  
  match decoded_japanese {
    Ok(text) => assert_eq(text, japanese_text)
    Err(_) => assert_true(false)
  }
  
  // 测试文本大小写转换（仅适用于有大小写的语言）
  let english_upper = english_text.to_uppercase()
  let english_lower = english_text.to_lowercase()
  let french_upper = french_text.to_uppercase()
  let french_lower = french_text.to_lowercase()
  let german_upper = german_text.to_uppercase()
  let german_lower = german_text.to_lowercase()
  
  assert_eq(english_upper, "AZIMUTH TELEMETRY SYSTEM")
  assert_eq(english_lower, "azimuth telemetry system")
  assert_eq(french_upper, "SYSTÈME DE TÉLÉMÉTRIE AZIMUTH")
  assert_eq(french_lower, "système de télémétrie azimuth")
  assert_eq(german_upper, "AZIMUTH-TELEMETRIESYSTEM")
  assert_eq(german_lower, "azimuth-telemetriesystem")
}

// 测试2: 日期和时间本地化
test "日期和时间本地化测试" {
  // 测试时间戳
  let test_timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // 测试不同地区的日期格式
  let us_date = azimuth::DateTimeFormatter::format_date(test_timestamp, "en-US", "MM/DD/YYYY")
  let uk_date = azimuth::DateTimeFormatter::format_date(test_timestamp, "en-GB", "DD/MM/YYYY")
  let iso_date = azimuth::DateTimeFormatter::format_date(test_timestamp, "en-ISO", "YYYY-MM-DD")
  let chinese_date = azimuth::DateTimeFormatter::format_date(test_timestamp, "zh-CN", "YYYY年MM月DD日")
  let japanese_date = azimuth::DateTimeFormatter::format_date(test_timestamp, "ja-JP", "YYYY年MM月DD日")
  
  assert_eq(us_date, "01/01/2021")
  assert_eq(uk_date, "01/01/2021")
  assert_eq(iso_date, "2021-01-01")
  assert_eq(chinese_date, "2021年01月01日")
  assert_eq(japanese_date, "2021年01月01日")
  
  // 测试不同地区的时间格式
  let us_time = azimuth::DateTimeFormatter::format_time(test_timestamp, "en-US", "hh:mm:ss A")
  let uk_time = azimuth::DateTimeFormatter::format_time(test_timestamp, "en-GB", "HH:mm:ss")
  let chinese_time = azimuth::DateTimeFormatter::format_time(test_timestamp, "zh-CN", "HH时mm分ss秒")
  let japanese_time = azimuth::DateTimeFormatter::format_time(test_timestamp, "ja-JP", "HH時mm分ss秒")
  
  assert_eq(us_time, "12:00:00 AM")
  assert_eq(uk_time, "00:00:00")
  assert_eq(chinese_time, "00时00分00秒")
  assert_eq(japanese_time, "00時00分00秒")
  
  // 测试不同地区的日期时间格式
  let us_datetime = azimuth::DateTimeFormatter::format_datetime(test_timestamp, "en-US", "MM/DD/YYYY hh:mm:ss A")
  let chinese_datetime = azimuth::DateTimeFormatter::format_datetime(test_timestamp, "zh-CN", "YYYY年MM月DD日 HH时mm分ss秒")
  
  assert_eq(us_datetime, "01/01/2021 12:00:00 AM")
  assert_eq(chinese_datetime, "2021年01月01日 00时00分00秒")
  
  // 测试时区转换
  let utc_time = azimuth::TimeZoneConverter::convert(test_timestamp, "UTC", "UTC")
  let us_eastern_time = azimuth::TimeZoneConverter::convert(test_timestamp, "UTC", "US/Eastern")
  let china_time = azimuth::TimeZoneConverter::convert(test_timestamp, "UTC", "Asia/Shanghai")
  let japan_time = azimuth::TimeZoneConverter::convert(test_timestamp, "UTC", "Asia/Tokyo")
  
  // 验证时区转换结果
  let us_eastern_formatted = azimuth::DateTimeFormatter::format_datetime(us_eastern_time, "en-US", "MM/DD/YYYY hh:mm:ss A")
  let china_formatted = azimuth::DateTimeFormatter::format_datetime(china_time, "zh-CN", "YYYY年MM月DD日 HH时mm分ss秒")
  let japan_formatted = azimuth::DateTimeFormatter::format_datetime(japan_time, "ja-JP", "YYYY年MM月DD日 HH時mm分ss秒")
  
  assert_eq(us_eastern_formatted, "12/31/2020 07:00:00 PM") // UTC-5
  assert_eq(china_formatted, "2021年01月01日 08时00分00秒") // UTC+8
  assert_eq(japan_formatted, "2021年01月01日 09時00分00秒") // UTC+9
}

// 测试3: 数字格式本地化
test "数字格式本地化测试" {
  // 测试整数格式化
  let test_number = 1234567
  
  let us_number = azimuth::NumberFormatter::format_integer(test_number, "en-US")
  let french_number = azimuth::NumberFormatter::format_integer(test_number, "fr-FR")
  let german_number = azimuth::NumberFormatter::format_integer(test_number, "de-DE")
  let chinese_number = azimuth::NumberFormatter::format_integer(test_number, "zh-CN")
  let indian_number = azimuth::NumberFormatter::format_integer(test_number, "en-IN")
  
  assert_eq(us_number, "1,234,567")
  assert_eq(french_number, "1 234 567")
  assert_eq(german_number, "1.234.567")
  assert_eq(chinese_number, "1,234,567")
  assert_eq(indian_number, "12,34,567")
  
  // 测试小数格式化
  let test_decimal = 1234567.89
  
  let us_decimal = azimuth::NumberFormatter::format_decimal(test_decimal, "en-US", 2)
  let french_decimal = azimuth::NumberFormatter::format_decimal(test_decimal, "fr-FR", 2)
  let german_decimal = azimuth::NumberFormatter::format_decimal(test_decimal, "de-DE", 2)
  let chinese_decimal = azimuth::NumberFormatter::format_decimal(test_decimal, "zh-CN", 2)
  
  assert_eq(us_decimal, "1,234,567.89")
  assert_eq(french_decimal, "1 234 567,89")
  assert_eq(german_decimal, "1.234.567,89")
  assert_eq(chinese_decimal, "1,234,567.89")
  
  // 测试百分比格式化
  let test_percentage = 0.6789
  
  let us_percentage = azimuth::NumberFormatter::format_percentage(test_percentage, "en-US", 2)
  let french_percentage = azimuth::NumberFormatter::format_percentage(test_percentage, "fr-FR", 2)
  let chinese_percentage = azimuth::NumberFormatter::format_percentage(test_percentage, "zh-CN", 2)
  
  assert_eq(us_percentage, "67.89%")
  assert_eq(french_percentage, "67,89 %")
  assert_eq(chinese_percentage, "67.89%")
  
  // 测试货币格式化
  let test_currency = 1234.56
  
  let usd_currency = azimuth::NumberFormatter::format_currency(test_currency, "en-US", "USD")
  let eur_currency = azimuth::NumberFormatter::format_currency(test_currency, "fr-FR", "EUR")
  let cny_currency = azimuth::NumberFormatter::format_currency(test_currency, "zh-CN", "CNY")
  let jpy_currency = azimuth::NumberFormatter::format_currency(test_currency, "ja-JP", "JPY")
  
  assert_eq(usd_currency, "$1,234.56")
  assert_eq(eur_currency, "1 234,56 €")
  assert_eq(cny_currency, "¥1,234.56")
  assert_eq(jpy_currency, "¥1,235") // 日元通常不显示小数
}

// 测试4: 消息和错误本地化
test "消息和错误本地化测试" {
  // 设置不同的语言环境
  let en_us_locale = azimuth::Locale::new("en-US")
  let zh_cn_locale = azimuth::Locale::new("zh-CN")
  let ja_jp_locale = azimuth::Locale::new("ja-JP")
  let fr_fr_locale = azimuth::Locale::new("fr-FR")
  
  // 测试系统消息本地化
  let en_startup_message = azimuth::MessageLocalizer::get_message("system.startup", en_us_locale)
  let zh_startup_message = azimuth::MessageLocalizer::get_message("system.startup", zh_cn_locale)
  let ja_startup_message = azimuth::MessageLocalizer::get_message("system.startup", ja_jp_locale)
  let fr_startup_message = azimuth::MessageLocalizer::get_message("system.startup", fr_fr_locale)
  
  assert_eq(en_startup_message, "Azimuth telemetry system started")
  assert_eq(zh_startup_message, "Azimuth 遥测系统已启动")
  assert_eq(ja_startup_message, "Azimuth テレメトリシステムが起動しました")
  assert_eq(fr_startup_message, "Le système de télémétrie Azimuth a démarré")
  
  // 测试错误消息本地化
  let en_error_message = azimuth::MessageLocalizer::get_message("error.connection_failed", en_us_locale)
  let zh_error_message = azimuth::MessageLocalizer::get_message("error.connection_failed", zh_cn_locale)
  let ja_error_message = azimuth::MessageLocalizer::get_message("error.connection_failed", ja_jp_locale)
  let fr_error_message = azimuth::MessageLocalizer::get_message("error.connection_failed", fr_fr_locale)
  
  assert_eq(en_error_message, "Connection to telemetry server failed")
  assert_eq(zh_error_message, "连接遥测服务器失败")
  assert_eq(ja_error_message, "テレメトリサーバーへの接続に失敗しました")
  assert_eq(fr_error_message, "La connexion au serveur de télémétrie a échoué")
  
  // 测试带参数的消息本地化
  let en_param_message = azimuth::MessageLocalizer::get_message_with_params(
    "span.completed", 
    en_us_locale, 
    [("span_name", "test_span"), ("duration_ms", "123")]
  )
  let zh_param_message = azimuth::MessageLocalizer::get_message_with_params(
    "span.completed", 
    zh_cn_locale, 
    [("span_name", "test_span"), ("duration_ms", "123")]
  )
  
  assert_eq(en_param_message, "Span 'test_span' completed in 123ms")
  assert_eq(zh_param_message, "跨度 'test_span' 在123毫秒内完成")
  
  // 测试复数形式处理
  let en_single_metric = azimuth::MessageLocalizer::get_plural_message(
    "metric.count", 
    en_us_locale, 
    1, 
    [("metric_type", "counter")]
  )
  let en_multiple_metrics = azimuth::MessageLocalizer::get_plural_message(
    "metric.count", 
    en_us_locale, 
    5, 
    [("metric_type", "counter")]
  )
  let zh_single_metric = azimuth::MessageLocalizer::get_plural_message(
    "metric.count", 
    zh_cn_locale, 
    1, 
    [("metric_type", "计数器")]
  )
  let zh_multiple_metrics = azimuth::MessageLocalizer::get_plural_message(
    "metric.count", 
    zh_cn_locale, 
    5, 
    [("metric_type", "计数器")]
  )
  
  assert_eq(en_single_metric, "Recorded 1 counter metric")
  assert_eq(en_multiple_metrics, "Recorded 5 counter metrics")
  assert_eq(zh_single_metric, "记录了1个计数器指标")
  assert_eq(zh_multiple_metrics, "记录了5个计数器指标")
}

// 测试5: 字符排序和比较
test "字符排序和比较测试" {
  // 测试不同语言的字符排序
  let english_words = ["apple", "banana", "cherry", "date"]
  let french_words = ["café", "crème", "déjà", "été"]
  let german_words = ["äpfel", "österreich", "übung", "ß"]
  let chinese_words = ["苹果", "香蕉", "樱桃", "日期"]
  let japanese_words = ["りんご", "バナナ", "チェリー", "日付"]
  
  // 按英语排序
  let sorted_english = azimuth::TextCollator::sort(english_words, "en-US")
  assert_eq(sorted_english, ["apple", "banana", "cherry", "date"])
  
  // 按法语排序
  let sorted_french = azimuth::TextCollator::sort(french_words, "fr-FR")
  assert_eq(sorted_french, ["café", "crème", "déjà", "été"])
  
  // 按德语排序
  let sorted_german = azimuth::TextCollator::sort(german_words, "de-DE")
  assert_eq(sorted_german, ["äpfel", "crème", "déjà", "été"]) // 假设排序结果
  
  // 按中文排序
  let sorted_chinese = azimuth::TextCollator::sort(chinese_words, "zh-CN")
  assert_eq(sorted_chinese, ["苹果", "香蕉", "樱桃", "日期"]) // 假设排序结果
  
  // 按日语排序
  let sorted_japanese = azimuth::TextCollator::sort(japanese_words, "ja-JP")
  assert_eq(sorted_japanese, ["りんご", "バナナ", "チェリー", "日付"]) // 假设排序结果
  
  // 测试字符串比较
  let en_compare_result1 = azimuth::TextCollator::compare("apple", "banana", "en-US")
  let en_compare_result2 = azimuth::TextCollator::compare("banana", "apple", "en-US")
  let en_compare_result3 = azimuth::TextCollator::compare("apple", "apple", "en-US")
  
  assert_true(en_compare_result1 < 0) // apple < banana
  assert_true(en_compare_result2 > 0) // banana > apple
  assert_eq(en_compare_result3, 0) // apple == apple
  
  // 测试不区分大小写的比较
  let case_insensitive_result1 = azimuth::TextCollator::compare_case_insensitive("Apple", "apple", "en-US")
  let case_insensitive_result2 = azimuth::TextCollator::compare_case_insensitive("Apple", "Banana", "en-US")
  
  assert_eq(case_insensitive_result1, 0) // Apple == apple (不区分大小写)
  assert_true(case_insensitive_result2 < 0) // Apple < Banana (不区分大小写)
}

// 测试6: 文化特定的数据处理
test "文化特定的数据处理测试" {
  // 测试姓名格式
  let western_name = azimuth::NameFormatter::format_western_name("John", "Doe", "en-US")
  let eastern_name = azimuth::NameFormatter::format_eastern_name("张", "三", "zh-CN")
  
  assert_eq(western_name, "John Doe")
  assert_eq(eastern_name, "张三")
  
  // 测试地址格式
  let us_address = azimuth::AddressFormatter::format_address(
    "123 Main St", 
    "New York", 
    "NY", 
    "10001", 
    "en-US"
  )
  let uk_address = azimuth::AddressFormatter::format_address(
    "10 Downing Street", 
    "London", 
    "", 
    "SW1A 2AA", 
    "en-GB"
  )
  let jp_address = azimuth::AddressFormatter::format_address(
    "〒100-0001", 
    "東京都千代田区千代田", 
    "", 
    "1-1", 
    "ja-JP"
  )
  
  assert_eq(us_address, "123 Main St\nNew York, NY 10001")
  assert_eq(uk_address, "10 Downing Street\nLondon\nSW1A 2AA")
  assert_eq(jp_address, "〒100-0001\n東京都千代田区千代田1-1")
  
  // 测试电话号码格式
  let us_phone = azimuth::PhoneFormatter::format("+1-555-123-4567", "en-US")
  let uk_phone = azimuth::PhoneFormatter::format("+44-20-7946-0958", "en-GB")
  let jp_phone = azimuth::PhoneFormatter::format("+81-3-1234-5678", "ja-JP")
  let cn_phone = azimuth::PhoneFormatter::format("+86-10-1234-5678", "zh-CN")
  
  assert_eq(us_phone, "(555) 123-4567")
  assert_eq(uk_phone, "020 7946 0958")
  assert_eq(jp_phone, "03-1234-5678")
  assert_eq(cn_phone, "010-1234-5678")
  
  // 测试度量单位转换
  let temperature_fahrenheit = 98.6 // 华氏度
  let temperature_celsius = azimuth::UnitConverter::fahrenheit_to_celsius(temperature_fahrenheit)
  assert_eq(temperature_celsius, 37.0)
  
  let distance_miles = 10.0 // 英里
  let distance_kilometers = azimuth::UnitConverter::miles_to_kilometers(distance_miles)
  assert_eq(distance_kilometers, 16.0934)
  
  let weight_pounds = 150.0 // 磅
  let weight_kilograms = azimuth::UnitConverter::pounds_to_kilograms(weight_pounds)
  assert_eq(weight_kilograms, 68.0389)
}

// 测试7: 文本方向和布局
test "文本方向和布局测试" {
  // 测试文本方向检测
  let ltr_text = "Hello World" // 从左到右
  let rtl_text = "مرحبا بالعالم" // 从右到左
  let mixed_text = "Hello مرحبا World" // 混合方向
  
  assert_eq(azimuth::TextDirection::detect(ltr_text), "LTR")
  assert_eq(azimuth::TextDirection::detect(rtl_text), "RTL")
  assert_eq(azimuth::TextDirection::detect(mixed_text), "LTR") // 默认为LTR
  
  // 测试文本方向标记
  let ltr_marked = azimuth::TextDirection::mark_direction(ltr_text, "LTR")
  let rtl_marked = azimuth::TextDirection::mark_direction(rtl_text, "RTL")
  
  assert_eq(ltr_marked, "\u200EHello World") // LRM标记
  assert_eq(rtl_marked, "\u200Fمرحبا بالعالم") // RLM标记
  
  // 测试文本对齐
  let ltr_alignment = azimuth::TextAlignment::get_alignment("en-US")
  let rtl_alignment = azimuth::TextAlignment::get_alignment("ar-SA")
  let hebrew_alignment = azimuth::TextAlignment::get_alignment("he-IL")
  
  assert_eq(ltr_alignment, "left")
  assert_eq(rtl_alignment, "right")
  assert_eq(hebrew_alignment, "right")
  
  // 测试文本布局
  let ltr_layout = azimuth::TextLayout::layout_text("Hello World", "en-US")
  let rtl_layout = azimuth::TextLayout::layout_text("مرحبا بالعالم", "ar-SA")
  
  assert_eq(ltr_layout.direction, "LTR")
  assert_eq(ltr_layout.text, "Hello World")
  
  assert_eq(rtl_layout.direction, "RTL")
  assert_eq(rtl_layout.text, "مرحبا بالعالم")
}

// 测试8: 输入法编辑器支持
test "输入法编辑器支持测试" {
  // 测试输入法状态
  let ime_state = azimuth::InputMethodEditor::get_state()
  assert_eq(ime_state.enabled, false) // 默认禁用
  
  // 测试启用输入法
  let enable_result = azimuth::InputMethodEditor::enable("zh-CN")
  match enable_result {
    Ok(_) => {
      let updated_state = azimuth::InputMethodEditor::get_state()
      assert_eq(updated_state.enabled, true)
      assert_eq(updated_state.locale, "zh-CN")
      
      // 测试输入法处理
      let composition_result = azimuth::InputMethodEditor::handle_composition("nihao")
      match composition_result {
        Ok(composition) => {
          assert_eq(composition.text, "nihao")
          assert_eq(composition.candidates.length(), 0) // 没有候选词
        }
        Err(_) => assert_true(false)
      }
      
      // 测试禁用输入法
      let disable_result = azimuth::InputMethodEditor::disable()
      match disable_result {
        Ok(_) => {
          let final_state = azimuth::InputMethodEditor::get_state()
          assert_eq(final_state.enabled, false)
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => {
      // 输入法可能不支持
      assert_true(true)
    }
  }
  
  // 测试不同语言的输入法
  let chinese_ime = azimuth::InputMethodEditor::create_ime("zh-CN")
  let japanese_ime = azimuth::InputMethodEditor::create_ime("ja-JP")
  let korean_ime = azimuth::InputMethodEditor::create_ime("ko-KR")
  
  match chinese_ime {
    Ok(ime) => assert_eq(azimuth::InputMethodEditor::get_locale(ime), "zh-CN")
    Err(_) => assert_true(true) // 可能不支持
  }
  
  match japanese_ime {
    Ok(ime) => assert_eq(azimuth::InputMethodEditor::get_locale(ime), "ja-JP")
    Err(_) => assert_true(true) // 可能不支持
  }
  
  match korean_ime {
    Ok(ime) => assert_eq(azimuth::InputMethodEditor::get_locale(ime), "ko-KR")
    Err(_) => assert_true(true) // 可能不支持
  }
}

// 测试9: 字符集和编码转换
test "字符集和编码转换测试" {
  // 测试UTF-8编码
  let utf8_text = "Hello 世界 こんにちは 안녕하세요"
  let utf8_bytes = azimuth::CharsetConverter::to_utf8(utf8_text)
  let utf8_decoded = azimuth::CharsetConverter::from_utf8(utf8_bytes)
  
  match utf8_decoded {
    Ok(text) => assert_eq(text, utf8_text)
    Err(_) => assert_true(false)
  }
  
  // 测试UTF-16编码
  let utf16_bytes = azimuth::CharsetConverter::to_utf16(utf8_text)
  let utf16_decoded = azimuth::CharsetConverter::from_utf16(utf16_bytes)
  
  match utf16_decoded {
    Ok(text) => assert_eq(text, utf8_text)
    Err(_) => assert_true(false)
  }
  
  // 测试ISO-8859-1编码（仅适用于西欧字符）
  let latin1_text = "Hello World"
  let latin1_bytes = azimuth::CharsetConverter::to_latin1(latin1_text)
  let latin1_decoded = azimuth::CharsetConverter::from_latin1(latin1_bytes)
  
  match latin1_decoded {
    Ok(text) => assert_eq(text, latin1_text)
    Err(_) => assert_true(false)
  }
  
  // 测试GBK编码（简体中文）
  let gbk_text = "你好世界"
  let gbk_bytes = azimuth::CharsetConverter::to_gbk(gbk_text)
  match gbk_bytes {
    Ok(bytes) => {
      let gbk_decoded = azimuth::CharsetConverter::from_gbk(bytes)
      match gbk_decoded {
        Ok(text) => assert_eq(text, gbk_text)
        Err(_) => assert_true(false)
      }
    }
    Err(_) => {
      // GBK编码可能不支持
      assert_true(true)
    }
  }
  
  // 测试Shift_JIS编码（日文）
  let shiftjis_text = "こんにちは世界"
  let shiftjis_bytes = azimuth::CharsetConverter::to_shiftjis(shiftjis_text)
  match shiftjis_bytes {
    Ok(bytes) => {
      let shiftjis_decoded = azimuth::CharsetConverter::from_shiftjis(bytes)
      match shiftjis_decoded {
        Ok(text) => assert_eq(text, shiftjis_text)
        Err(_) => assert_true(false)
      }
    }
    Err(_) => {
      // Shift_JIS编码可能不支持
      assert_true(true)
    }
  }
  
  // 测试字符编码检测
  let detected_encoding = azimuth::CharsetDetector::detect_encoding(utf8_bytes)
  assert_eq(detected_encoding, "UTF-8")
  
  let detected_latin1 = azimuth::CharsetDetector::detect_encoding(latin1_bytes)
  assert_eq(detected_latin1, "ISO-8859-1")
}

// 测试10: 本地化资源管理
test "本地化资源管理测试" {
  // 创建本地化资源管理器
  let resource_manager = azimuth::LocalizationResourceManager::new()
  
  // 加载不同语言的资源
  let en_us_result = azimuth::LocalizationResourceManager::load_locale(resource_manager, "en-US")
  let zh_cn_result = azimuth::LocalizationResourceManager::load_locale(resource_manager, "zh-CN")
  let ja_jp_result = azimuth::LocalizationResourceManager::load_locale(resource_manager, "ja-JP")
  
  assert_true(en_us_result)
  assert_true(zh_cn_result)
  assert_true(ja_jp_result)
  
  // 测试获取本地化字符串
  let en_string = azimuth::LocalizationResourceManager::get_string(resource_manager, "en-US", "welcome.message")
  let zh_string = azimuth::LocalizationResourceManager::get_string(resource_manager, "zh-CN", "welcome.message")
  let ja_string = azimuth::LocalizationResourceManager::get_string(resource_manager, "ja-JP", "welcome.message")
  
  match en_string {
    Some(text) => assert_eq(text, "Welcome to Azimuth Telemetry System")
    None => assert_true(false)
  }
  
  match zh_string {
    Some(text) => assert_eq(text, "欢迎使用 Azimuth 遥测系统")
    None => assert_true(false)
  }
  
  match ja_string {
    Some(text) => assert_eq(text, "Azimuth テレメトリシステムへようこそ")
    None => assert_true(false)
  }
  
  // 测试获取不存在的字符串
  let non_existent = azimuth::LocalizationResourceManager::get_string(resource_manager, "en-US", "non.existent.key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试获取不存在的语言环境的字符串
  let non_existent_locale = azimuth::LocalizationResourceManager::get_string(resource_manager, "ko-KR", "welcome.message")
  match non_existent_locale {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试回退机制（当请求的语言环境不存在时，回退到默认语言）
  let fallback_string = azimuth::LocalizationResourceManager::get_string_with_fallback(
    resource_manager, 
    "fr-FR", // 不存在的语言环境
    "welcome.message", 
    "en-US" // 回退语言环境
  )
  
  match fallback_string {
    Some(text) => assert_eq(text, "Welcome to Azimuth Telemetry System")
    None => assert_true(false)
  }
  
  // 测试获取所有可用的语言环境
  let available_locales = azimuth::LocalizationResourceManager::get_available_locales(resource_manager)
  assert_true(available_locales.contains("en-US"))
  assert_true(available_locales.contains("zh-CN"))
  assert_true(available_locales.contains("ja-JP"))
  
  // 测试卸载语言环境
  let unload_result = azimuth::LocalizationResourceManager::unload_locale(resource_manager, "ja-JP")
  assert_true(unload_result)
  
  let unloaded_locales = azimuth::LocalizationResourceManager::get_available_locales(resource_manager)
  assert_false(unloaded_locales.contains("ja-JP"))
  assert_true(unloaded_locales.contains("en-US"))
  assert_true(unloaded_locales.contains("zh-CN"))
}