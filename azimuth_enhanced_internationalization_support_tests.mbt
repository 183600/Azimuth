// Azimuth 国际化支持增强测试
// 专注于多语言支持、本地化、字符编码、时区处理和文化适配

// 测试1: 多语言文本处理
test "多语言文本处理测试" {
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  
  // 配置支持的语言
  let supported_locales = [
    "en-US",    // 英语（美国）
    "zh-CN",    // 简体中文
    "zh-TW",    // 繁体中文
    "ja-JP",    // 日语
    "ko-KR",    // 韩语
    "fr-FR",    // 法语
    "de-DE",    // 德语
    "es-ES",    // 西班牙语
    "ru-RU",    // 俄语
    "ar-SA"     // 阿拉伯语
  ]
  
  // 注册支持的语言
  for locale in supported_locales {
    I18nManager::register_locale(i18n_manager, locale)
  }
  
  // 定义多语言资源
  let message_resources = {
    "en-US": {
      "welcome": "Welcome to Azimuth",
      "goodbye": "Goodbye",
      "error_occurred": "An error occurred: {error}",
      "items_count": "{count, plural, one{# item} other{# items}}",
      "date_format": "MM/DD/YYYY",
      "time_format": "h:mm:ss A"
    },
    "zh-CN": {
      "welcome": "欢迎使用 Azimuth",
      "goodbye": "再见",
      "error_occurred": "发生错误：{error}",
      "items_count": "{count, plural, other{# 项}}",
      "date_format": "YYYY年MM月DD日",
      "time_format": "AHH:mm:ss"
    },
    "zh-TW": {
      "welcome": "歡迎使用 Azimuth",
      "goodbye": "再見",
      "error_occurred": "發生錯誤：{error}",
      "items_count": "{count, plural, other{# 項}}",
      "date_format": "YYYY年MM月DD日",
      "time_format": "AHH:mm:ss"
    },
    "ja-JP": {
      "welcome": "Azimuthへようこそ",
      "goodbye": "さようなら",
      "error_occurred": "エラーが発生しました：{error}",
      "items_count": "{count, plural, other{# 項目}}",
      "date_format": "YYYY/MM/DD",
      "time_format": "AHH:mm:ss"
    },
    "ko-KR": {
      "welcome": "Azimuth에 오신 것을 환영합니다",
      "goodbye": "안녕히 가세요",
      "error_occurred": "오류가 발생했습니다: {error}",
      "items_count": "{count, plural, other{# 항목}}",
      "date_format": "YYYY. MM. DD.",
      "time_format": "A HH:mm:ss"
    },
    "fr-FR": {
      "welcome": "Bienvenue sur Azimuth",
      "goodbye": "Au revoir",
      "error_occurred": "Une erreur s'est produite : {error}",
      "items_count": "{count, plural, one{# élément} other{# éléments}}",
      "date_format": "DD/MM/YYYY",
      "time_format": "HH:mm:ss"
    },
    "de-DE": {
      "welcome": "Willkommen bei Azimuth",
      "goodbye": "Auf Wiedersehen",
      "error_occurred": "Ein Fehler ist aufgetreten: {error}",
      "items_count": "{count, plural, one{# Element} other{# Elemente}}",
      "date_format": "DD.MM.YYYY",
      "time_format": "HH:mm:ss"
    },
    "es-ES": {
      "welcome": "Bienvenido a Azimuth",
      "goodbye": "Adiós",
      "error_occurred": "Ocurrió un error: {error}",
      "items_count": "{count, plural, one{# elemento} other{# elementos}}",
      "date_format": "DD/MM/YYYY",
      "time_format": "H:mm:ss"
    },
    "ru-RU": {
      "welcome": "Добро пожаловать в Azimuth",
      "goodbye": "До свидания",
      "error_occurred": "Произошла ошибка: {error}",
      "items_count": "{count, plural, one{# элемент} few{# элемента} many{# элементов} other{# элементов}}",
      "date_format": "DD.MM.YYYY",
      "time_format": "H:mm:ss"
    },
    "ar-SA": {
      "welcome": "مرحبا بك في Azimuth",
      "goodbye": "وداعا",
      "error_occurred": "حدث خطأ: {error}",
      "items_count": "{count, plural, one{# عنصر} other{# عناصر}}",
      "date_format": "DD/MM/YYYY",
      "time_format": "H:mm:ss"
    }
  }
  
  // 加载多语言资源
  for (locale, resources) in message_resources.entries() {
    I18nManager::load_resources(i18n_manager, locale, resources)
  }
  
  // 设置当前语言
  I18nManager::set_locale(i18n_manager, "en-US")
  
  // 测试基本消息翻译
  let welcome_en = I18nManager::translate(i18n_manager, "welcome")
  assert_eq(welcome_en, "Welcome to Azimuth")
  
  let goodbye_en = I18nManager::translate(i18n_manager, "goodbye")
  assert_eq(goodbye_en, "Goodbye")
  
  // 测试中文翻译
  I18nManager::set_locale(i18n_manager, "zh-CN")
  
  let welcome_zh = I18nManager::translate(i18n_manager, "welcome")
  assert_eq(welcome_zh, "欢迎使用 Azimuth")
  
  let goodbye_zh = I18nManager::translate(i18n_manager, "goodbye")
  assert_eq(goodbye_zh, "再见")
  
  // 测试日语翻译
  I18nManager::set_locale(i18n_manager, "ja-JP")
  
  let welcome_ja = I18nManager::translate(i18n_manager, "welcome")
  assert_eq(welcome_ja, "Azimuthへようこそ")
  
  // 测试韩语翻译
  I18nManager::set_locale(i18n_manager, "ko-KR")
  
  let welcome_ko = I18nManager::translate(i18n_manager, "welcome")
  assert_eq(welcome_ko, "Azimuth에 오신 것을 환영합니다")
  
  // 测试参数化消息
  I18nManager::set_locale(i18n_manager, "en-US")
  
  let error_en = I18nManager::translate_with_params(i18n_manager, "error_occurred", [("error", "Connection timeout")])
  assert_eq(error_en, "An error occurred: Connection timeout")
  
  // 测试复数形式
  let items_one_en = I18nManager::translate_with_params(i18n_manager, "items_count", [("count", 1)])
  assert_eq(items_one_en, "1 item")
  
  let items_many_en = I18nManager::translate_with_params(i18n_manager, "items_count", [("count", 5)])
  assert_eq(items_many_en, "5 items")
  
  // 测试中文复数形式
  I18nManager::set_locale(i18n_manager, "zh-CN")
  
  let items_zh = I18nManager::translate_with_params(i18n_manager, "items_count", [("count", 5)])
  assert_eq(items_zh, "5 项")
  
  // 测试俄语复数形式
  I18nManager::set_locale(i18n_manager, "ru-RU")
  
  let items_one_ru = I18nManager::translate_with_params(i18n_manager, "items_count", [("count", 1)])
  assert_eq(items_one_ru, "1 элемент")
  
  let items_few_ru = I18nManager::translate_with_params(i18n_manager, "items_count", [("count", 3)])
  assert_eq(items_few_ru, "3 элемента")
  
  let items_many_ru = I18nManager::translate_with_params(i18n_manager, "items_count", [("count", 7)])
  assert_eq(items_many_ru, "7 элементов")
  
  // 测试回退机制
  I18nManager::set_locale(i18n_manager, "en-GB")  // 不支持的语言
  
  // 应该回退到基础语言（en）
  let welcome_fallback = I18nManager::translate(i18n_manager, "welcome")
  assert_eq(welcome_fallback, "Welcome to Azimuth")
  
  // 测试缺失键的回退
  I18nManager::set_locale(i18n_manager, "en-US")
  
  let missing_key = I18nManager::translate(i18n_manager, "nonexistent_key")
  assert_eq(missing_key, "nonexistent_key")  // 应该返回键名
  
  // 测试RTL语言支持
  I18nManager::set_locale(i18n_manager, "ar-SA")
  
  let is_rtl = I18nManager::is_rtl_locale(i18n_manager)
  assert_true(is_rtl)
  
  let welcome_ar = I18nManager::translate(i18n_manager, "welcome")
  assert_eq(welcome_ar, "مرحبا بك في Azimuth")
  
  // 测试LTR语言
  I18nManager::set_locale(i18n_manager, "en-US")
  
  let is_ltr = I18nManager::is_ltr_locale(i18n_manager)
  assert_true(is_ltr)
  
  // 获取语言信息
  let locale_info = I18nManager::get_locale_info(i18n_manager, "zh-CN")
  assert_eq(locale_info.language, "zh")
  assert_eq(locale_info.country, "CN")
  assert_eq(locale_info.display_name, "Chinese (Simplified)")
  assert_true(locale_info.rtl == false)
}

// 测试2: 字符编码和转换
test "字符编码和转换测试" {
  // 创建编码管理器
  let encoding_manager = EncodingManager::new()
  
  // 测试UTF-8编码
  let utf8_text = "Hello 世界 🌍 Azimuth"
  let utf8_bytes = EncodingManager::encode_utf8(encoding_manager, utf8_text)
  
  // 验证UTF-8编码
  assert_true(utf8_bytes.length() > utf8_text.length())  // UTF-8字节应该比字符多
  
  // 测试UTF-8解码
  let decoded_text = EncodingManager::decode_utf8(encoding_manager, utf8_bytes)
  assert_eq(decoded_text, utf8_text)
  
  // 测试不同语言的UTF-8编码
  let multilingual_texts = [
    "English text with émojis 🎉",
    "中文文本包含表情符号 🎊",
    "日本語テキストと絵文字 🎌",
    "한국어 텍스트와 이모지 🎋",
    "Texte français avec des émojis 🎈",
    "Deutscher Text mit Emojis 🎀",
    "Texto español con emojis 🎁",
    "Русский текст с эмодзи 🎂",
    "نص عربي مع الرموز التعبيرية 🎃"
  ]
  
  for text in multilingual_texts {
    let encoded = EncodingManager::encode_utf8(encoding_manager, text)
    let decoded = EncodingManager::decode_utf8(encoding_manager, encoded)
    assert_eq(decoded, text)
  }
  
  // 测试UTF-16编码
  let utf16_bytes = EncodingManager::encode_utf16(encoding_manager, utf8_text)
  let utf16_decoded = EncodingManager::decode_utf16(encoding_manager, utf16_bytes)
  assert_eq(utf16_decoded, utf8_text)
  
  // 测试UTF-32编码
  let utf32_bytes = EncodingManager::encode_utf32(encoding_manager, utf8_text)
  let utf32_decoded = EncodingManager::decode_utf32(encoding_manager, utf32_bytes)
  assert_eq(utf32_decoded, utf8_text)
  
  // 测试ASCII编码（仅适用于ASCII字符）
  let ascii_text = "Hello Azimuth"
  let ascii_bytes = EncodingManager::encode_ascii(encoding_manager, ascii_text)
  let ascii_decoded = EncodingManager::decode_ascii(encoding_manager, ascii_bytes)
  assert_eq(ascii_decoded, ascii_text)
  
  // 测试非ASCII字符的ASCII编码（应该失败或替换）
  let non_ascii_text = "Hello 世界"
  let ascii_encode_result = EncodingManager::encode_ascii_safe(encoding_manager, non_ascii_text)
  assert_true(ascii_encode_result.is_ok())
  
  let safe_ascii_bytes = ascii_encode_result.unwrap()
  let safe_ascii_decoded = EncodingManager::decode_ascii(encoding_manager, safe_ascii_bytes)
  assert_eq(safe_ascii_decoded, "Hello ??")  // 非ASCII字符被替换
  
  // 测试编码检测
  let test_bytes = EncodingManager::encode_utf8(encoding_manager, "Hello 世界")
  let detected_encoding = EncodingManager::detect_encoding(encoding_manager, test_bytes)
  assert_eq(detected_encoding, "utf-8")
  
  // 测试编码转换
  let conversion_result = EncodingManager::convert_encoding(
    encoding_manager,
    test_bytes,
    "utf-8",
    "utf-16"
  )
  assert_true(conversion_result.is_ok())
  
  let converted_bytes = conversion_result.unwrap()
  let back_converted = EncodingManager::convert_encoding(
    encoding_manager,
    converted_bytes,
    "utf-16",
    "utf-8"
  )
  assert_true(back_converted.is_ok())
  
  let back_converted_bytes = back_converted.unwrap()
  let final_text = EncodingManager::decode_utf8(encoding_manager, back_converted_bytes)
  assert_eq(final_text, "Hello 世界")
  
  // 测试BOM（字节顺序标记）处理
  let utf8_with_bom = EncodingManager::add_bom_utf8(encoding_manager, test_bytes)
  let has_bom = EncodingManager::has_bom(encoding_manager, utf8_with_bom)
  assert_true(has_bom)
  
  let bom_removed = EncodingManager::remove_bom(encoding_manager, utf8_with_bom)
  let bom_removed_decoded = EncodingManager::decode_utf8(encoding_manager, bom_removed)
  assert_eq(bom_removed_decoded, "Hello 世界")
  
  // 测试Unicode规范化
  let unicode_text = "café"  // 包含组合字符
  let normalized_nfc = EncodingManager::normalize_unicode(encoding_manager, unicode_text, "NFC")
  let normalized_nfd = EncodingManager::normalize_unicode(encoding_manager, unicode_text, "NFD")
  
  // NFC和NFD应该看起来不同但语义相同
  assert_eq(normalized_nfc, "café")
  assert_true(normalized_nfd.length() >= normalized_nfc.length())
  
  // 测试大小写转换（考虑语言特定规则）
  let case_tests = [
    ("hello", "en-US", "Hello"),
    ("hello", "tr-TR", "Hello"),  // 土耳其语有特殊的大小写规则
    ("i", "tr-TR", "İ"),         // 土耳其语的小写i到大写是İ
    ("İ", "tr-TR", "i"),         // 土耳其语的大写İ到小写是i
    ("ß", "de-DE", "SS")          // 德语的ß到大写是SS
  ]
  
  for (text, locale, expected) in case_tests {
    let upper_result = EncodingManager::uppercase(encoding_manager, text, locale)
    assert_eq(upper_result, expected)
  }
  
  // 测试字符串比较（考虑语言特定规则）
  let comparison_tests = [
    ("café", "cafe", "en-US", false),   // 在英语中不同
    ("café", "cafe", "fr-FR", false),   // 在法语中也不同
    ("Å", "A", "sv-SE", true),          // 在瑞典语中相同
    ("ß", "ss", "de-DE", true)          // 在德语中相同
  ]
  
  for (text1, text2, locale, expected) in comparison_tests {
    let is_equal = EncodingManager::locale_compare(encoding_manager, text1, text2, locale)
    assert_eq(is_equal, expected)
  }
  
  // 获取编码统计
  let encoding_stats = EncodingManager::get_statistics(encoding_manager)
  assert_true(encoding_stats.total_encodings > 0)
  assert_true(encoding_stats.total_decodings > 0)
  assert_true(encoding_stats.encoding_conversions > 0)
  assert_true(encoding_stats.avg_encoding_time_ms > 0)
}

// 测试3: 日期和时间本地化
test "日期和时间本地化测试" {
  // 创建日期时间本地化器
  let datetime_localizer = DateTimeLocalizer::new()
  
  // 定义测试时间戳
  let test_timestamp = 1640995200000  // 2022-01-01 00:00:00 UTC
  
  // 测试不同时区
  let timezones = [
    "UTC",           // 协调世界时
    "America/New_York",  // 美国东部时间
    "America/Los_Angeles",  // 美国西部时间
    "Europe/London",  // 伦敦时间
    "Europe/Paris",   // 巴黎时间
    "Asia/Tokyo",     // 东京时间
    "Asia/Shanghai",  // 上海时间
    "Asia/Seoul",     // 首尔时间
    "Australia/Sydney",  // 悉尼时间
  ]
  
  // 测试时区转换
  for timezone in timezones {
    let localized_time = DateTimeLocalizer::convert_timezone(datetime_localizer, test_timestamp, "UTC", timezone)
    assert_true(localized_time.year >= 2021 and localized_time.year <= 2022)
    assert_true(localized_time.month >= 1 and localized_time.month <= 12)
    assert_true(localized_time.day >= 1 and localized_time.day <= 31)
  }
  
  // 测试日期格式化
  let date_formats = [
    ("en-US", "MM/DD/YYYY"),
    ("en-GB", "DD/MM/YYYY"),
    ("zh-CN", "YYYY年MM月DD日"),
    ("ja-JP", "YYYY/MM/DD"),
    ("de-DE", "DD.MM.YYYY"),
    ("fr-FR", "DD/MM/YYYY"),
    ("ko-KR", "YYYY. MM. DD."),
    ("ru-RU", "DD.MM.YYYY"),
    ("ar-SA", "DD/MM/YYYY")
  ]
  
  for (locale, format) in date_formats {
    let formatted_date = DateTimeLocalizer::format_date(datetime_localizer, test_timestamp, locale, format)
    assert_true(formatted_date.length() > 0)
    
    // 验证年份在格式化日期中
    assert_true(formatted_date.contains("2022"))
  }
  
  // 测试时间格式化
  let time_formats = [
    ("en-US", "h:mm:ss A"),
    ("en-GB", "HH:mm:ss"),
    ("zh-CN", "AHH:mm:ss"),
    ("ja-JP", "AHH:mm:ss"),
    ("de-DE", "HH:mm:ss"),
    ("fr-FR", "HH:mm:ss"),
    ("ko-KR", "A HH:mm:ss"),
    ("ru-RU", "H:mm:ss"),
    ("ar-SA", "H:mm:ss")
  ]
  
  for (locale, format) in time_formats {
    let formatted_time = DateTimeLocalizer::format_time(datetime_localizer, test_timestamp, locale, format)
    assert_true(formatted_time.length() > 0)
  }
  
  // 测试日期时间组合格式化
  let datetime_formats = [
    ("en-US", "MM/DD/YYYY h:mm:ss A"),
    ("zh-CN", "YYYY年MM月DD日 AHH:mm:ss"),
    ("ja-JP", "YYYY/MM/DD AHH:mm:ss"),
    ("de-DE", "DD.MM.YYYY HH:mm:ss"),
    ("ko-KR", "YYYY. MM. DD. A HH:mm:ss")
  ]
  
  for (locale, format) in datetime_formats {
    let formatted_datetime = DateTimeLocalizer::format_datetime(datetime_localizer, test_timestamp, locale, format)
    assert_true(formatted_datetime.length() > 0)
    assert_true(formatted_datetime.contains("2022"))
  }
  
  // 测试相对时间格式化
  let now_timestamp = test_timestamp
  let past_timestamp = test_timestamp - 3600000  // 1小时前
  let future_timestamp = test_timestamp + 3600000  // 1小时后
  
  // 测试英语相对时间
  let relative_past_en = DateTimeLocalizer::format_relative_time(datetime_localizer, past_timestamp, now_timestamp, "en-US")
  assert_eq(relative_past_en, "1 hour ago")
  
  let relative_future_en = DateTimeLocalizer::format_relative_time(datetime_localizer, future_timestamp, now_timestamp, "en-US")
  assert_eq(relative_future_en, "in 1 hour")
  
  // 测试中文相对时间
  let relative_past_zh = DateTimeLocalizer::format_relative_time(datetime_localizer, past_timestamp, now_timestamp, "zh-CN")
  assert_eq(relative_past_zh, "1小时前")
  
  let relative_future_zh = DateTimeLocalizer::format_relative_time(datetime_localizer, future_timestamp, now_timestamp, "zh-CN")
  assert_eq(relative_future_zh, "1小时后")
  
  // 测试日语相对时间
  let relative_past_ja = DateTimeLocalizer::format_relative_time(datetime_localizer, past_timestamp, now_timestamp, "ja-JP")
  assert_eq(relative_past_ja, "1時間前")
  
  // 测试韩语相对时间
  let relative_past_ko = DateTimeLocalizer::format_relative_time(datetime_localizer, past_timestamp, now_timestamp, "ko-KR")
  assert_eq(relative_past_ko, "1시간 전")
  
  // 测试月份和星期名称本地化
  let month_names_en = DateTimeLocalizer::get_month_names(datetime_localizer, "en-US")
  assert_eq(month_names_en[0], "January")
  assert_eq(month_names_en[6], "July")
  assert_eq(month_names_en[11], "December")
  
  let month_names_zh = DateTimeLocalizer::get_month_names(datetime_localizer, "zh-CN")
  assert_eq(month_names_zh[0], "一月")
  assert_eq(month_names_zh[6], "七月")
  assert_eq(month_names_zh[11], "十二月")
  
  let weekday_names_en = DateTimeLocalizer::get_weekday_names(datetime_localizer, "en-US")
  assert_eq(weekday_names_en[0], "Sunday")
  assert_eq(weekday_names_en[6], "Saturday")
  
  let weekday_names_zh = DateTimeLocalizer::get_weekday_names(datetime_localizer, "zh-CN")
  assert_eq(weekday_names_zh[0], "星期日")
  assert_eq(weekday_names_zh[6], "星期六")
  
  // 测试日历系统
  let calendar_tests = [
    ("en-US", "gregorian"),
    ("zh-CN", "gregorian"),
    ("ja-JP", "gregorian"),
    ("ar-SA", "gregorian"),
    ("th-TH", "buddhist")  // 泰国使用佛历
  ]
  
  for (locale, expected_calendar) in calendar_tests {
    let calendar = DateTimeLocalizer::get_calendar_system(datetime_localizer, locale)
    assert_eq(calendar, expected_calendar)
  }
  
  // 测试佛历年份
  let buddhist_year = DateTimeLocalizer::get_year_in_calendar(datetime_localizer, test_timestamp, "th-TH")
  assert_eq(buddhist_year, 2022 + 543)  // 佛历 = 公历 + 543
  
  // 测试数字和日期本地化
  let number_localizer = NumberLocalizer::new()
  
  // 测试数字格式化
  let number_tests = [
    (1234.567, "en-US", "1,234.567"),
    (1234.567, "de-DE", "1.234,567"),
    (1234.567, "fr-FR", "1 234,567"),
    (1234.567, "zh-CN", "1,234.567"),
    (1234.567, "ja-JP", "1,234.567")
  ]
  
  for (number, locale, expected) in number_tests {
    let formatted_number = NumberLocalizer::format_number(number_localizer, number, locale)
    assert_eq(formatted_number, expected)
  }
  
  // 测试货币格式化
  let currency_tests = [
    (1234.56, "en-US", "USD", "$1,234.56"),
    (1234.56, "de-DE", "EUR", "1.234,56 €"),
    (1234.56, "fr-FR", "EUR", "1 234,56 €"),
    (1234.56, "ja-JP", "JPY", "￥1,235"),
    (1234.56, "zh-CN", "CNY", "¥1,234.56")
  ]
  
  for (amount, locale, currency, expected) in currency_tests {
    let formatted_currency = NumberLocalizer::format_currency(number_localizer, amount, locale, currency)
    assert_eq(formatted_currency, expected)
  }
  
  // 获取日期时间本地化统计
  let datetime_stats = DateTimeLocalizer::get_statistics(datetime_localizer)
  assert_true(datetime_stats.total_formatting_operations > 0)
  assert_true(datetime_stats.total_timezone_conversions > 0)
  assert_true(datetime_stats.total_relative_time_formats > 0)
  assert_true(datetime_stats.avg_formatting_time_ms > 0)
}

// 测试4: 文化适配和本地化规则
test "文化适配和本地化规则测试" {
  // 创建文化适配管理器
  let culture_adapter = CultureAdapter::new()
  
  // 定义文化规则
  let culture_rules = {
    "en-US": {
      date_format: "MM/DD/YYYY",
      time_format: "h:mm A",
      number_format: {
        decimal_separator: ".",
        thousands_separator: ",",
        currency_symbol: "$",
        currency_position: "prefix"
      },
      address_format: "{street}\n{city}, {state} {zipcode}",
      name_format: "{given_name} {family_name}",
      reading_direction: "ltr",
      weekend_days: [6, 0]  // 周六、周日
      first_day_of_week: 0   // 周日
    },
    "zh-CN": {
      date_format: "YYYY年MM月DD日",
      time_format: "AHH:mm",
      number_format: {
        decimal_separator: ".",
        thousands_separator: ",",
        currency_symbol: "¥",
        currency_position: "prefix"
      },
      address_format: "{zipcode} {state}{city}{street}",
      name_format: "{family_name}{given_name}",
      reading_direction: "ltr",
      weekend_days: [5, 6],  // 周六、周日
      first_day_of_week: 1    // 周一
    },
    "ja-JP": {
      date_format: "YYYY/MM/DD",
      time_format: "AHH:mm",
      number_format: {
        decimal_separator: ".",
        thousands_separator: ",",
        currency_symbol: "￥",
        currency_position: "prefix"
      },
      address_format: "〒{zipcode}\n{state}{city}{street}",
      name_format: "{family_name} {given_name}",
      reading_direction: "ltr",
      weekend_days: [5, 6],  // 周六、周日
      first_day_of_week: 0    // 周日
    },
    "ar-SA": {
      date_format: "DD/MM/YYYY",
      time_format: "H:mm",
      number_format: {
        decimal_separator: ".",
        thousands_separator: ",",
        currency_symbol: "ر.س",
        currency_position: "suffix"
      },
      address_format: "{street}\n{city} {zipcode}\n{state}",
      name_format: "{given_name} {family_name}",
      reading_direction: "rtl",
      weekend_days: [4, 5],  // 周五、周六
      first_day_of_week: 6    // 周六
    },
    "de-DE": {
      date_format: "DD.MM.YYYY",
      time_format: "HH:mm",
      number_format: {
        decimal_separator: ",",
        thousands_separator: ".",
        currency_symbol: "€",
        currency_position: "suffix"
      },
      address_format: "{street}\n{zipcode} {city}\n{state}",
      name_format: "{given_name} {family_name}",
      reading_direction: "ltr",
      weekend_days: [5, 6],  // 周六、周日
      first_day_of_week: 1    // 周一
    }
  }
  
  // 加载文化规则
  for (locale, rules) in culture_rules.entries() {
    CultureAdapter::load_culture_rules(culture_adapter, locale, rules)
  }
  
  // 测试地址格式化
  let address_data = {
    street: "123 Main St",
    city: "New York",
    state: "NY",
    zipcode: "10001"
  }
  
  // 测试英语地址格式
  let address_en = CultureAdapter::format_address(culture_adapter, "en-US", address_data)
  assert_eq(address_en, "123 Main St\nNew York, NY 10001")
  
  // 测试中文地址格式
  let address_zh = CultureAdapter::format_address(culture_adapter, "zh-CN", address_data)
  assert_eq(address_zh, "10001 NYNew York123 Main St")
  
  // 测试日语地址格式
  let address_ja = CultureAdapter::format_address(culture_adapter, "ja-JP", address_data)
  assert_eq(address_ja, "〒10001\nNYNew York123 Main St")
  
  // 测试阿拉伯语地址格式
  let address_ar = CultureAdapter::format_address(culture_adapter, "ar-SA", address_data)
  assert_eq(address_ar, "123 Main St\nNew York 10001\nNY")
  
  // 测试姓名格式化
  let name_data = {
    given_name: "John",
    family_name: "Doe"
  }
  
  // 测试英语姓名格式
  let name_en = CultureAdapter::format_name(culture_adapter, "en-US", name_data)
  assert_eq(name_en, "John Doe")
  
  // 测试中文姓名格式
  let name_zh = CultureAdapter::format_name(culture_adapter, "zh-CN", name_data)
  assert_eq(name_zh, "DoeJohn")
  
  // 测试日语姓名格式
  let name_ja = CultureAdapter::format_name(culture_adapter, "ja-JP", name_data)
  assert_eq(name_ja, "Doe John")
  
  // 测试货币位置
  let currency_tests = [
    ("en-US", 1234.56, "$1,234.56"),
    ("de-DE", 1234.56, "1.234,56 €"),
    ("ar-SA", 1234.56, "1,234.56 ر.س")
  ]
  
  for (locale, amount, expected) in currency_tests {
    let formatted_currency = CultureAdapter::format_currency(culture_adapter, locale, amount)
    assert_eq(formatted_currency, expected)
  }
  
  // 测试数字格式
  let number_tests = [
    ("en-US", 1234567.89, "1,234,567.89"),
    ("de-DE", 1234567.89, "1.234.567,89"),
    ("zh-CN", 1234567.89, "1,234,567.89")
  ]
  
  for (locale, number, expected) in number_tests {
    let formatted_number = CultureAdapter::format_number(culture_adapter, locale, number)
    assert_eq(formatted_number, expected)
  }
  
  // 测试周末和工作日判断
  let weekend_tests = [
    ("en-US", 6, true),   // 周六是周末
    ("en-US", 0, true),   // 周日是周末
    ("en-US", 3, false),  // 周三不是周末
    ("zh-CN", 5, true),   // 周六是周末
    ("zh-CN", 6, true),   // 周日是周末
    ("zh-CN", 3, false),  // 周三不是周末
    ("ar-SA", 4, true),   // 周五是周末
    ("ar-SA", 5, true),   // 周六是周末
    ("ar-SA", 3, false)   // 周三不是周末
  ]
  
  for (locale, day, expected) in weekend_tests {
    let is_weekend = CultureAdapter::is_weekend(culture_adapter, locale, day)
    assert_eq(is_weekend, expected)
  }
  
  // 测试工作日判断
  let workday_tests = [
    ("en-US", 1, true),   // 周一是工作日
    ("en-US", 6, false),  // 周六不是工作日
    ("zh-CN", 1, true),   // 周一是工作日
    ("zh-CN", 6, false),  // 周六不是工作日
    ("ar-SA", 1, true),   // 周一是工作日
    ("ar-SA", 5, false)   // 周五不是工作日
  ]
  
  for (locale, day, expected) in workday_tests {
    let is_workday = CultureAdapter::is_workday(culture_adapter, locale, day)
    assert_eq(is_workday, expected)
  }
  
  // 测试阅读方向
  let direction_tests = [
    ("en-US", "ltr"),
    ("zh-CN", "ltr"),
    ("ja-JP", "ltr"),
    ("ar-SA", "rtl"),
    ("he-IL", "rtl")
  ]
  
  for (locale, expected) in direction_tests {
    let direction = CultureAdapter::get_reading_direction(culture_adapter, locale)
    assert_eq(direction, expected)
  }
  
  // 测试文本对齐
  let alignment_tests = [
    ("en-US", "left"),
    ("zh-CN", "left"),
    ("ja-JP", "left"),
    ("ar-SA", "right"),
    ("he-IL", "right")
  ]
  
  for (locale, expected) in alignment_tests {
    let alignment = CultureAdapter::get_text_alignment(culture_adapter, locale)
    assert_eq(alignment, expected)
  }
  
  // 测试文化特定排序
  let sorting_tests = [
    (["apple", "banana", "cherry"], "en-US"),
    (["苹果", "香蕉", "樱桃"], "zh-CN"),
    (["りんご", "ばなな", "さくらんぼ"], "ja-JP")
  ]
  
  for (items, locale) in sorting_tests {
    let sorted = CultureAdapter::sort_strings(culture_adapter, items, locale)
    assert_eq(sorted.length(), items.length())
    
    // 验证排序是否正确（具体顺序取决于语言的排序规则）
    for i in 0..=sorted.length() - 2 {
      assert_true(CultureAdapter::compare_strings(culture_adapter, sorted[i], sorted[i + 1], locale) <= 0)
    }
  }
  
  // 测试文化特定搜索
  let search_tests = [
    ("café", "cafe", "en-US", false),  // 在英语中不同
    ("café", "cafe", "fr-FR", false),  // 在法语中也不同
    ("Å", "A", "sv-SE", true),         // 在瑞典语中相同
    ("ß", "ss", "de-DE", true)         // 在德语中相同
  ]
  
  for (text, pattern, locale, expected) in search_tests {
    let matches = CultureAdapter::locale_search(culture_adapter, text, pattern, locale)
    assert_eq(matches, expected)
  }
  
  // 获取文化适配统计
  let culture_stats = CultureAdapter::get_statistics(culture_adapter)
  assert_true(culture_stats.total_formatting_operations > 0)
  assert_true(culture_stats.total_validations > 0)
  assert_true(culture_stats.total_sorting_operations > 0)
  assert_true(culture_stats.avg_formatting_time_ms > 0)
}

// 测试5: 国际化性能和优化
test "国际化性能和优化测试" {
  // 创建国际化性能测试器
  let i18n_performance = I18nPerformance::new()
  
  // 配置性能测试参数
  I18nPerformance::configure(i18n_performance, {
    test_iterations: 10000,
    concurrent_threads: 4,
    memory_tracking: true,
    cache_analysis: true
  })
  
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  
  // 注册大量语言
  let large_locale_list = []
  for i in 0..=50 {
    large_locale_list.push("test-locale-" + i.to_string())
  }
  
  for locale in large_locale_list {
    I18nManager::register_locale(i18n_manager, locale)
  }
  
  // 加载大量资源
  let large_resources = {}
  for locale in large_locale_list {
    let locale_resources = {}
    for j in 0..=100 {
      locale_resources["key_" + j.to_string()] = "value_" + j.to_string() + "_in_" + locale
    }
    large_resources[locale] = locale_resources
  }
  
  for (locale, resources) in large_resources.entries() {
    I18nManager::load_resources(i18n_manager, locale, resources)
  }
  
  // 测试翻译性能
  let translation_start = Time::now()
  
  for i in 0..=10000 {
    let locale = large_locale_list[i % large_locale_list.length()]
    let key = "key_" + (i % 100).to_string()
    I18nManager::set_locale(i18n_manager, locale)
    I18nManager::translate(i18n_manager, key)
  }
  
  let translation_end = Time::now()
  let translation_time = translation_end - translation_start
  
  // 验证翻译性能
  assert_true(translation_time < 5000)  // 10000次翻译应在5秒内完成
  
  // 测试参数化翻译性能
  let param_translation_start = Time::now()
  
  for i in 0..=5000 {
    let locale = large_locale_list[i % large_locale_list.length()]
    let key = "key_" + (i % 100).to_string()
    let params = [("param1", "value1"), ("param2", "value2")]
    I18nManager::set_locale(i18n_manager, locale)
    I18nManager::translate_with_params(i18n_manager, key, params)
  }
  
  let param_translation_end = Time::now()
  let param_translation_time = param_translation_end - param_translation_start
  
  // 验证参数化翻译性能
  assert_true(param_translation_time < 3000)  // 5000次参数化翻译应在3秒内完成
  
  // 测试缓存效果
  let cache_test_start = Time::now()
  
  // 第一次翻译（缓存未命中）
  I18nManager::set_locale(i18n_manager, "test-locale-0")
  let first_translation = I18nManager::translate(i18n_manager, "key_0")
  
  // 第二次相同翻译（缓存命中）
  let second_translation = I18nManager::translate(i18n_manager, "key_0")
  
  let cache_test_end = Time::now()
  let cache_test_time = cache_test_end - cache_test_start
  
  // 验证缓存效果
  assert_eq(first_translation, second_translation)
  assert_true(cache_test_time < 10)  // 缓存命中应该非常快
  
  // 测试内存使用
  let memory_before = I18nPerformance::get_memory_usage(i18n_performance)
  
  // 执行大量操作
  for i in 0..=1000 {
    let locale = large_locale_list[i % large_locale_list.length()]
    I18nManager::set_locale(i18n_manager, locale)
    
    for j in 0..=100 {
      let key = "key_" + j.to_string()
      I18nManager::translate(i18n_manager, key)
    }
  }
  
  let memory_after = I18nPerformance::get_memory_usage(i18n_performance)
  
  // 验证内存使用合理
  assert_true(memory_after.heap_used_mb > memory_before.heap_used_mb)
  assert_true(memory_after.heap_used_mb < memory_before.heap_used_mb + 100)  // 内存增长应小于100MB
  
  // 测试并发性能
  let concurrent_start = Time::now()
  
  let concurrent_tasks = []
  for thread_id in 0..=4 {
    let task = fn() {
      for i in 0..=2000 {
        let locale = large_locale_list[i % large_locale_list.length()]
        let key = "key_" + (i % 100).to_string()
        I18nManager::set_locale(i18n_manager, locale)
        I18nManager::translate(i18n_manager, key)
      }
      thread_id
    }
    concurrent_tasks.push(task)
  }
  
  // 执行并发任务
  let concurrent_results = []
  for task in concurrent_tasks {
    let result = task()
    concurrent_results.push(result)
  }
  
  let concurrent_end = Time::now()
  let concurrent_time = concurrent_end - concurrent_start
  
  // 验证并发性能
  assert_eq(concurrent_results.length(), 5)
  assert_true(concurrent_time < 3000)  // 并发执行应该更快
  
  // 测试资源预加载性能
  let preload_start = Time::now()
  
  I18nManager::preload_resources(i18n_manager, large_locale_list)
  
  let preload_end = Time::now()
  let preload_time = preload_end - preload_start
  
  // 验证预加载性能
  assert_true(preload_time < 1000)  // 预加载应在1秒内完成
  
  // 测试热切换性能
  let hot_swap_start = Time::now()
  
  for i in 0..=1000 {
    let locale = large_locale_list[i % large_locale_list.length()]
    I18nManager::set_locale(i18n_manager, locale)
  }
  
  let hot_swap_end = Time::now()
  let hot_swap_time = hot_swap_end - hot_swap_start
  
  // 验证热切换性能
  assert_true(hot_swap_time < 500)  // 1000次语言切换应在500ms内完成
  
  // 测试资源压缩效果
  let original_size = I18nPerformance::calculate_resource_size(i18n_performance, large_resources)
  
  I18nManager::enable_compression(i18n_manager, true)
  let compressed_size = I18nPerformance::get_compressed_size(i18n_performance)
  
  // 验证压缩效果
  assert_true(compressed_size < original_size)
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  assert_true(compression_ratio < 0.8)  // 压缩率应至少达到20%
  
  // 获取性能报告
  let performance_report = I18nPerformance::generate_report(i18n_performance, {
    include_translation_metrics: true,
    include_memory_metrics: true,
    include_cache_metrics: true,
    include_recommendations: true
  })
  
  // 验证性能报告
  assert_true(performance_report.translation_metrics.total_translations > 0)
  assert_true(performance_report.translation_metrics.avg_translation_time_ms > 0)
  assert_true(performance_report.memory_metrics.peak_heap_used_mb > 0)
  assert_true(performance_report.cache_metrics.cache_hit_rate > 0)
  assert_true(performance_report.recommendations.length() > 0)
}