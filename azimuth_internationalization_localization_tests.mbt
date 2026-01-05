// Azimuth Telemetry System - Internationalization and Localization Tests
// This file contains test cases for internationalization and localization features

// Test 1: Basic Internationalization Support
test "basic internationalization support" {
  // Initialize internationalization manager
  let i18n = I18nManager::new()
  
  // Load language resources
  let en_us_resource = LanguageResource {
    locale: "en-US",
    strings: [
      ("welcome", "Welcome to Azimuth Telemetry"),
      ("cpu_usage", "CPU Usage"),
      ("memory_usage", "Memory Usage"),
      ("error_occurred", "An error occurred"),
      ("settings", "Settings")
    ]
  }
  
  let zh_cn_resource = LanguageResource {
    locale: "zh-CN",
    strings: [
      ("welcome", "欢迎使用Azimuth遥测系统"),
      ("cpu_usage", "CPU使用率"),
      ("memory_usage", "内存使用率"),
      ("error_occurred", "发生错误"),
      ("settings", "设置")
    ]
  }
  
  let es_es_resource = LanguageResource {
    locale: "es-ES",
    strings: [
      ("welcome", "Bienvenido a Azimuth Telemetry"),
      ("cpu_usage", "Uso de CPU"),
      ("memory_usage", "Uso de Memoria"),
      ("error_occurred", "Ocurrió un error"),
      ("settings", "Configuración")
    ]
  }
  
  // Register language resources
  assert_true(i18n.register_resource(en_us_resource).is_ok())
  assert_true(i18n.register_resource(zh_cn_resource).is_ok())
  assert_true(i18n.register_resource(es_es_resource).is_ok())
  
  // Test English translations
  i18n.set_locale("en-US")
  assert_eq(i18n.translate("welcome"), "Welcome to Azimuth Telemetry")
  assert_eq(i18n.translate("cpu_usage"), "CPU Usage")
  assert_eq(i18n.translate("memory_usage"), "Memory Usage")
  assert_eq(i18n.translate("error_occurred"), "An error occurred")
  assert_eq(i18n.translate("settings"), "Settings")
  
  // Test Chinese translations
  i18n.set_locale("zh-CN")
  assert_eq(i18n.translate("welcome"), "欢迎使用Azimuth遥测系统")
  assert_eq(i18n.translate("cpu_usage"), "CPU使用率")
  assert_eq(i18n.translate("memory_usage"), "内存使用率")
  assert_eq(i18n.translate("error_occurred"), "发生错误")
  assert_eq(i18n.translate("settings"), "设置")
  
  // Test Spanish translations
  i18n.set_locale("es-ES")
  assert_eq(i18n.translate("welcome"), "Bienvenido a Azimuth Telemetry")
  assert_eq(i18n.translate("cpu_usage"), "Uso de CPU")
  assert_eq(i18n.translate("memory_usage"), "Uso de Memoria")
  assert_eq(i18n.translate("error_occurred"), "Ocurrió un error")
  assert_eq(i18n.translate("settings"), "Configuración")
  
  // Test fallback to default language
  i18n.set_locale("fr-FR")  // French not registered
  assert_eq(i18n.translate("welcome"), "Welcome to Azimuth Telemetry")  // Fallback to English
  
  // Test missing key fallback
  i18n.set_locale("zh-CN")
  assert_eq(i18n.translate("nonexistent_key"), "nonexistent_key")  // Return key if not found
}

// Test 2: Parameterized Translations
test "parameterized translations" {
  // Initialize internationalization manager
  let i18n = I18nManager::new()
  
  // Load language resources with parameters
  let en_us_resource = LanguageResource {
    locale: "en-US",
    strings: [
      ("items_count", "{count} items"),
      ("user_greeting", "Hello, {name}!"),
      ("error_with_code", "Error {code}: {message}"),
      ("percentage", "{value}%"),
      ("time_elapsed", "Time elapsed: {hours}h {minutes}m {seconds}s")
    ]
  }
  
  let zh_cn_resource = LanguageResource {
    locale: "zh-CN",
    strings: [
      ("items_count", "{count}个项目"),
      ("user_greeting", "你好，{name}！"),
      ("error_with_code", "错误 {code}：{message}"),
      ("percentage", "{value}%"),
      ("time_elapsed", "已用时间：{hours}小时{minutes}分钟{seconds}秒")
    ]
  }
  
  let ja_jp_resource = LanguageResource {
    locale: "ja-JP",
    strings: [
      ("items_count", "{count}アイテム"),
      ("user_greeting", "こんにちは、{name}さん！"),
      ("error_with_code", "エラー {code}: {message}"),
      ("percentage", "{value}%"),
      ("time_elapsed", "経過時間：{hours}時間{minutes}分{seconds}秒")
    ]
  }
  
  // Register language resources
  assert_true(i18n.register_resource(en_us_resource).is_ok())
  assert_true(i18n.register_resource(zh_cn_resource).is_ok())
  assert_true(i18n.register_resource(ja_jp_resource).is_ok())
  
  // Test English parameterized translations
  i18n.set_locale("en-US")
  
  let items_params = [("count", "42")]
  assert_eq(i18n.translate_with_params("items_count", items_params), "42 items")
  
  let greeting_params = [("name", "John")]
  assert_eq(i18n.translate_with_params("user_greeting", greeting_params), "Hello, John!")
  
  let error_params = [("code", "404"), ("message", "Not Found")]
  assert_eq(i18n.translate_with_params("error_with_code", error_params), "Error 404: Not Found")
  
  let percentage_params = [("value", "75.5")]
  assert_eq(i18n.translate_with_params("percentage", percentage_params), "75.5%")
  
  let time_params = [("hours", "2"), ("minutes", "30"), ("seconds", "45")]
  assert_eq(i18n.translate_with_params("time_elapsed", time_params), "Time elapsed: 2h 30m 45s")
  
  // Test Chinese parameterized translations
  i18n.set_locale("zh-CN")
  
  let zh_items_params = [("count", "42")]
  assert_eq(i18n.translate_with_params("items_count", zh_items_params), "42个项目")
  
  let zh_greeting_params = [("name", "张三")]
  assert_eq(i18n.translate_with_params("user_greeting", zh_greeting_params), "你好，张三！")
  
  let zh_error_params = [("code", "404"), ("message", "未找到")]
  assert_eq(i18n.translate_with_params("error_with_code", zh_error_params), "错误 404：未找到")
  
  // Test Japanese parameterized translations
  i18n.set_locale("ja-JP")
  
  let ja_items_params = [("count", "42")]
  assert_eq(i18n.translate_with_params("items_count", ja_items_params), "42アイテム")
  
  let ja_greeting_params = [("name", "田中")]
  assert_eq(i18n.translate_with_params("user_greeting", ja_greeting_params), "こんにちは、田中さん！")
  
  // Test missing parameters
  i18n.set_locale("en-US")
  let incomplete_params = [("count", "42")]  // Missing name parameter
  assert_eq(i18n.translate_with_params("user_greeting", incomplete_params), "Hello, {name}!")  // Unsubstituted parameter
  
  // Test extra parameters
  let extra_params = [("count", "42"), ("extra", "ignored")]
  assert_eq(i18n.translate_with_params("items_count", extra_params), "42 items")  // Extra parameter ignored
}

// Test 3: Pluralization Support
test "pluralization support" {
  // Initialize internationalization manager
  let i18n = I18nManager::new()
  
  // Load language resources with pluralization rules
  let en_us_resource = LanguageResource {
    locale: "en-US",
    strings: [
      ("items_count", PluralRule {
        zero: "No items",
        one: "{count} item",
        other: "{count} items"
      }),
      ("files_count", PluralRule {
        zero: "No files",
        one: "{count} file",
        other: "{count} files"
      }),
      ("messages_count", PluralRule {
        zero: "No messages",
        one: "{count} message",
        other: "{count} messages"
      })
    ]
  }
  
  let zh_cn_resource = LanguageResource {
    locale: "zh-CN",
    strings: [
      ("items_count", PluralRule {
        other: "{count}个项目"  // Chinese doesn't typically distinguish singular/plural
      }),
      ("files_count", PluralRule {
        other: "{count}个文件"
      }),
      ("messages_count", PluralRule {
        other: "{count}条消息"
      })
    ]
  }
  
  let ru_ru_resource = LanguageResource {
    locale: "ru-RU",
    strings: [
      ("items_count", PluralRule {
        one: "{count} предмет",
        few: "{count} предмета",
        many: "{count} предметов",
        other: "{count} предметов"
      }),
      ("files_count", PluralRule {
        one: "{count} файл",
        few: "{count} файла",
        many: "{count} файлов",
        other: "{count} файлов"
      })
    ]
  }
  
  // Register language resources
  assert_true(i18n.register_resource(en_us_resource).is_ok())
  assert_true(i18n.register_resource(zh_cn_resource).is_ok())
  assert_true(i18n.register_resource(ru_ru_resource).is_ok())
  
  // Test English pluralization
  i18n.set_locale("en-US")
  
  assert_eq(i18n.pluralize("items_count", 0), "No items")
  assert_eq(i18n.pluralize("items_count", 1), "1 item")
  assert_eq(i18n.pluralize("items_count", 42), "42 items")
  
  assert_eq(i18n.pluralize("files_count", 0), "No files")
  assert_eq(i18n.pluralize("files_count", 1), "1 file")
  assert_eq(i18n.pluralize("files_count", 42), "42 files")
  
  assert_eq(i18n.pluralize("messages_count", 0), "No messages")
  assert_eq(i18n.pluralize("messages_count", 1), "1 message")
  assert_eq(i18n.pluralize("messages_count", 42), "42 messages")
  
  // Test Chinese pluralization
  i18n.set_locale("zh-CN")
  
  assert_eq(i18n.pluralize("items_count", 0), "0个项目")
  assert_eq(i18n.pluralize("items_count", 1), "1个项目")
  assert_eq(i18n.pluralize("items_count", 42), "42个项目")
  
  assert_eq(i18n.pluralize("files_count", 0), "0个文件")
  assert_eq(i18n.pluralize("files_count", 1), "1个文件")
  assert_eq(i18n.pluralize("files_count", 42), "42个文件")
  
  // Test Russian pluralization
  i18n.set_locale("ru-RU")
  
  assert_eq(i18n.pluralize("items_count", 1), "1 предмет")
  assert_eq(i18n.pluralize("items_count", 2), "2 предмета")
  assert_eq(i18n.pluralize("items_count", 5), "5 предметов")
  assert_eq(i18n.pluralize("items_count", 11), "11 предметов")
  assert_eq(i18n.pluralize("items_count", 21), "21 предмет")
  assert_eq(i18n.pluralize("items_count", 22), "22 предмета")
  assert_eq(i18n.pluralize("items_count", 25), "25 предметов")
  
  assert_eq(i18n.pluralize("files_count", 1), "1 файл")
  assert_eq(i18n.pluralize("files_count", 2), "2 файла")
  assert_eq(i18n.pluralize("files_count", 5), "5 файлов")
  
  // Test fallback for missing plural forms
  i18n.set_locale("en-US")
  assert_eq(i18n.pluralize("nonexistent_key", 42), "nonexistent_key")
}

// Test 4: Date and Time Formatting
test "date and time formatting" {
  // Initialize internationalization manager
  let i18n = I18nManager::new()
  
  // Load locale-specific date/time formats
  let en_us_formats = LocaleFormats {
    locale: "en-US",
    date_formats: [
      ("short", "MM/dd/yyyy"),
      ("medium", "MMM d, yyyy"),
      ("long", "MMMM d, yyyy"),
      ("full", "EEEE, MMMM d, yyyy")
    ],
    time_formats: [
      ("short", "h:mm a"),
      ("medium", "h:mm:ss a"),
      ("long", "h:mm:ss a z"),
      ("full", "h:mm:ss a zzzz")
    ],
    datetime_formats: [
      ("short", "MM/dd/yyyy, h:mm a"),
      ("medium", "MMM d, yyyy, h:mm:ss a"),
      ("long", "MMMM d, yyyy 'at' h:mm:ss a z"),
      ("full", "EEEE, MMMM d, yyyy 'at' h:mm:ss a zzzz")
    ]
  }
  
  let zh_cn_formats = LocaleFormats {
    locale: "zh-CN",
    date_formats: [
      ("short", "yyyy/MM/dd"),
      ("medium", "yyyy年M月d日"),
      ("long", "yyyy年M月d日"),
      ("full", "yyyy年M月d日EEEE")
    ],
    time_formats: [
      ("short", "HH:mm"),
      ("medium", "HH:mm:ss"),
      ("long", "z HH:mm:ss"),
      ("full", "zzzz HH:mm:ss")
    ],
    datetime_formats: [
      ("short", "yyyy/MM/dd HH:mm"),
      ("medium", "yyyy年M月d日 HH:mm:ss"),
      ("long", "yyyy年M月d日 z HH:mm:ss"),
      ("full", "yyyy年M月d日EEEE zzzz HH:mm:ss")
    ]
  }
  
  let de_de_formats = LocaleFormats {
    locale: "de-DE",
    date_formats: [
      ("short", "dd.MM.yyyy"),
      ("medium", "d. MMM yyyy"),
      ("long", "d. MMMM yyyy"),
      ("full": "EEEE, d. MMMM yyyy")
    ],
    time_formats: [
      ("short", "HH:mm"),
      ("medium", "HH:mm:ss"),
      ("long", "HH:mm:ss z"),
      ("full", "HH:mm:ss zzzz")
    ],
    datetime_formats: [
      ("short", "dd.MM.yyyy, HH:mm"),
      ("medium", "d. MMM yyyy, HH:mm:ss"),
      ("long", "d. MMMM yyyy 'um' HH:mm:ss z"),
      ("full", "EEEE, d. MMMM yyyy 'um' HH:mm:ss zzzz")
    ]
  }
  
  // Register locale formats
  assert_true(i18n.register_formats(en_us_formats).is_ok())
  assert_true(i18n.register_formats(zh_cn_formats).is_ok())
  assert_true(i18n.register_formats(de_de_formats).is_ok())
  
  // Create test date/time
  let test_datetime = DateTime {
    year: 2023,
    month: 6,
    day: 15,
    hour: 14,
    minute: 30,
    second: 45,
    timezone: "UTC"
  }
  
  // Test English date formatting
  i18n.set_locale("en-US")
  
  assert_eq(i18n.format_date(test_datetime, "short"), "06/15/2023")
  assert_eq(i18n.format_date(test_datetime, "medium"), "Jun 15, 2023")
  assert_eq(i18n.format_date(test_datetime, "long"), "June 15, 2023")
  assert_eq(i18n.format_date(test_datetime, "full"), "Thursday, June 15, 2023")
  
  // Test English time formatting
  assert_eq(i18n.format_time(test_datetime, "short"), "2:30 PM")
  assert_eq(i18n.format_time(test_datetime, "medium"), "2:30:45 PM")
  assert_eq(i18n.format_time(test_datetime, "long"), "2:30:45 PM UTC")
  assert_eq(i18n.format_time(test_datetime, "full"), "2:30:45 PM UTC")
  
  // Test English datetime formatting
  assert_eq(i18n.format_datetime(test_datetime, "short"), "06/15/2023, 2:30 PM")
  assert_eq(i18n.format_datetime(test_datetime, "medium"), "Jun 15, 2023, 2:30:45 PM")
  assert_eq(i18n.format_datetime(test_datetime, "long"), "June 15, 2023 at 2:30:45 PM UTC")
  assert_eq(i18n.format_datetime(test_datetime, "full"), "Thursday, June 15, 2023 at 2:30:45 PM UTC")
  
  // Test Chinese date formatting
  i18n.set_locale("zh-CN")
  
  assert_eq(i18n.format_date(test_datetime, "short"), "2023/06/15")
  assert_eq(i18n.format_date(test_datetime, "medium"), "2023年6月15日")
  assert_eq(i18n.format_date(test_datetime, "long"), "2023年6月15日")
  assert_eq(i18n.format_date(test_datetime, "full"), "2023年6月15日星期四")
  
  // Test Chinese time formatting
  assert_eq(i18n.format_time(test_datetime, "short"), "14:30")
  assert_eq(i18n.format_time(test_datetime, "medium"), "14:30:45")
  assert_eq(i18n.format_time(test_datetime, "long"), "UTC 14:30:45")
  assert_eq(i18n.format_time(test_datetime, "full"), "UTC 14:30:45")
  
  // Test Chinese datetime formatting
  assert_eq(i18n.format_datetime(test_datetime, "short"), "2023/06/15 14:30")
  assert_eq(i18n.format_datetime(test_datetime, "medium"), "2023年6月15日 14:30:45")
  assert_eq(i18n.format_datetime(test_datetime, "long"), "2023年6月15日 UTC 14:30:45")
  assert_eq(i18n.format_datetime(test_datetime, "full"), "2023年6月15日星期四 UTC 14:30:45")
  
  // Test German date formatting
  i18n.set_locale("de-DE")
  
  assert_eq(i18n.format_date(test_datetime, "short"), "15.06.2023")
  assert_eq(i18n.format_date(test_datetime, "medium"), "15. Jun 2023")
  assert_eq(i18n.format_date(test_datetime, "long"), "15. Juni 2023")
  assert_eq(i18n.format_date(test_datetime, "full"), "Donnerstag, 15. Juni 2023")
  
  // Test German time formatting
  assert_eq(i18n.format_time(test_datetime, "short"), "14:30")
  assert_eq(i18n.format_time(test_datetime, "medium"), "14:30:45")
  assert_eq(i18n.format_time(test_datetime, "long"), "14:30:45 UTC")
  assert_eq(i18n.format_time(test_datetime, "full"), "14:30:45 UTC")
  
  // Test German datetime formatting
  assert_eq(i18n.format_datetime(test_datetime, "short"), "15.06.2023, 14:30")
  assert_eq(i18n.format_datetime(test_datetime, "medium"), "15. Jun 2023, 14:30:45")
  assert_eq(i18n.format_datetime(test_datetime, "long"), "15. Juni 2023 um 14:30:45 UTC")
  assert_eq(i18n.format_datetime(test_datetime, "full"), "Donnerstag, 15. Juni 2023 um 14:30:45 UTC")
  
  // Test relative time formatting
  i18n.set_locale("en-US")
  let now = DateTime { year: 2023, month: 6, day: 15, hour: 15, minute: 0, second: 0, timezone: "UTC" }
  
  let past_datetime = DateTime { year: 2023, month: 6, day: 15, hour: 14, minute: 30, second: 0, timezone: "UTC" }
  assert_eq(i18n.format_relative_time(past_datetime, now), "30 minutes ago")
  
  let future_datetime = DateTime { year: 2023, month: 6, day: 15, hour: 16, minute: 0, second: 0, timezone: "UTC" }
  assert_eq(i18n.format_relative_time(future_datetime, now), "in 1 hour")
  
  i18n.set_locale("zh-CN")
  assert_eq(i18n.format_relative_time(past_datetime, now), "30分钟前")
  assert_eq(i18n.format_relative_time(future_datetime, now), "1小时后")
  
  i18n.set_locale("de-DE")
  assert_eq(i18n.format_relative_time(past_datetime, now), "vor 30 Minuten")
  assert_eq(i18n.format_relative_time(future_datetime, now), "in 1 Stunde")
}

// Test 5: Number Formatting
test "number formatting" {
  // Initialize internationalization manager
  let i18n = I18nManager::new()
  
  // Load locale-specific number formats
  let en_us_formats = LocaleNumberFormats {
    locale: "en-US",
    decimal_separator: ".",
    thousands_separator: ",",
    currency_symbol: "$",
    currency_prefix: true,
    percent_suffix: "%"
  }
  
  let de_de_formats = LocaleNumberFormats {
    locale: "de-DE",
    decimal_separator: ",",
    thousands_separator: ".",
    currency_symbol: "€",
    currency_prefix: false,
    percent_suffix: "%"
  }
  
  let zh_cn_formats = LocaleNumberFormats {
    locale: "zh-CN",
    decimal_separator: ".",
    thousands_separator: ",",
    currency_symbol: "¥",
    currency_prefix: true,
    percent_suffix: "%"
  }
  
  let ar_sa_formats = LocaleNumberFormats {
    locale: "ar-SA",
    decimal_separator: "٫",  // Arabic decimal separator
    thousands_separator: "٬",  // Arabic thousands separator
    currency_symbol: "ر.س",
    currency_prefix: false,
    percent_suffix: "٪"  // Arabic percent sign
  }
  
  // Register locale number formats
  assert_true(i18n.register_number_formats(en_us_formats).is_ok())
  assert_true(i18n.register_number_formats(de_de_formats).is_ok())
  assert_true(i18n.register_number_formats(zh_cn_formats).is_ok())
  assert_true(i18n.register_number_formats(ar_sa_formats).is_ok())
  
  // Test English number formatting
  i18n.set_locale("en-US")
  
  assert_eq(i18n.format_decimal(1234.5678, 2), "1,234.57")
  assert_eq(i18n.format_decimal(1234567.89, 0), "1,234,568")
  assert_eq(i18n.format_decimal(0.1234, 4), "0.1234")
  
  assert_eq(i18n.format_currency(1234.56), "$1,234.56")
  assert_eq(i18n.format_currency(1234567.89), "$1,234,567.89")
  
  assert_eq(i18n.format_percent(0.1234, 1), "12.3%")
  assert_eq(i18n.format_percent(0.5678, 2), "56.78%")
  
  // Test German number formatting
  i18n.set_locale("de-DE")
  
  assert_eq(i18n.format_decimal(1234.5678, 2), "1.234,57")
  assert_eq(i18n.format_decimal(1234567.89, 0), "1.234.568")
  assert_eq(i18n.format_decimal(0.1234, 4), "0,1234")
  
  assert_eq(i18n.format_currency(1234.56), "1.234,56 €")
  assert_eq(i18n.format_currency(1234567.89), "1.234.567,89 €")
  
  assert_eq(i18n.format_percent(0.1234, 1), "12,3%")
  assert_eq(i18n.format_percent(0.5678, 2), "56,78%")
  
  // Test Chinese number formatting
  i18n.set_locale("zh-CN")
  
  assert_eq(i18n.format_decimal(1234.5678, 2), "1,234.57")
  assert_eq(i18n.format_decimal(1234567.89, 0), "1,234,568")
  assert_eq(i18n.format_decimal(0.1234, 4), "0.1234")
  
  assert_eq(i18n.format_currency(1234.56), "¥1,234.56")
  assert_eq(i18n.format_currency(1234567.89), "¥1,234,567.89")
  
  assert_eq(i18n.format_percent(0.1234, 1), "12.3%")
  assert_eq(i18n.format_percent(0.5678, 2), "56.78%")
  
  // Test Arabic number formatting
  i18n.set_locale("ar-SA")
  
  assert_eq(i18n.format_decimal(1234.5678, 2), "1٬234٫57")
  assert_eq(i18n.format_decimal(1234567.89, 0), "1٬234٬568")
  assert_eq(i18n.format_decimal(0.1234, 4), "0٫1234")
  
  assert_eq(i18n.format_currency(1234.56), "1٬234٫56 ر.س")
  assert_eq(i18n.format_currency(1234567.89), "1٬234٬567٫89 ر.س")
  
  assert_eq(i18n.format_percent(0.1234, 1), "12٫3٪")
  assert_eq(i18n.format_percent(0.5678, 2), "56٫78٪")
  
  // Test unit formatting
  i18n.set_locale("en-US")
  
  assert_eq(i18n.format_unit(1024, "bytes"), "1,024 bytes")
  assert_eq(i18n.format_unit(1048576, "bytes"), "1,048,576 bytes")
  assert_eq(i18n.format_unit(1234.56, "KB"), "1,234.56 KB")
  assert_eq(i18n.format_unit(56.78, "MB/s"), "56.78 MB/s")
  
  i18n.set_locale("de-DE")
  
  assert_eq(i18n.format_unit(1024, "bytes"), "1.024 bytes")
  assert_eq(i18n.format_unit(1048576, "bytes"), "1.048.576 bytes")
  assert_eq(i18n.format_unit(1234.56, "KB"), "1.234,56 KB")
  assert_eq(i18n.format_unit(56.78, "MB/s"), "56,78 MB/s")
  
  // Test compact number formatting
  i18n.set_locale("en-US")
  
  assert_eq(i18n.format_compact(1000), "1K")
  assert_eq(i18n.format_compact(1500), "1.5K")
  assert_eq(i18n.format_compact(1000000), "1M")
  assert_eq(i18n.format_compact(2500000), "2.5M")
  assert_eq(i18n.format_compact(1000000000), "1B")
  
  i18n.set_locale("zh-CN")
  
  assert_eq(i18n.format_compact(1000), "1千")
  assert_eq(i18n.format_compact(1500), "1.5千")
  assert_eq(i18n.format_compact(10000), "1万")
  assert_eq(i18n.format_compact(15000), "1.5万")
  assert_eq(i18n.format_compact(1000000), "100万")
  assert_eq(i18n.format_compact(2500000), "250万")
  
  i18n.set_locale("de-DE")
  
  assert_eq(i18n.format_compact(1000), "1 Tsd.")
  assert_eq(i18n.format_compact(1500), "1,5 Tsd.")
  assert_eq(i18n.format_compact(1000000), "1 Mio.")
  assert_eq(i18n.format_compact(2500000), "2,5 Mio.")
  assert_eq(i18n.format_compact(1000000000), "1 Mrd.")
}

// Test 6: Text Direction and Layout
test "text direction and layout" {
  // Initialize internationalization manager
  let i18n = I18nManager::new()
  
  // Load locale-specific text direction settings
  let ltr_locales = ["en-US", "zh-CN", "de-DE", "fr-FR", "es-ES", "ja-JP"]
  let rtl_locales = ["ar-SA", "he-IL", "fa-IR", "ur-PK"]
  
  // Register text direction settings
  for locale in ltr_locales {
    assert_true(i18n.register_text_direction(locale, "LTR").is_ok())
  }
  
  for locale in rtl_locales {
    assert_true(i18n.register_text_direction(locale, "RTL").is_ok())
  }
  
  // Test LTR text direction
  i18n.set_locale("en-US")
  assert_eq(i18n.get_text_direction(), "LTR")
  assert_eq(i18n.get_text_align(), "left")
  assert_eq(i18n.get_margin_start(), "margin-left")
  assert_eq(i18n.get_margin_end(), "margin-right")
  assert_eq(i18n.get_padding_start(), "padding-left")
  assert_eq(i18n.get_padding_end(), "padding-right")
  
  // Test RTL text direction
  i18n.set_locale("ar-SA")
  assert_eq(i18n.get_text_direction(), "RTL")
  assert_eq(i18n.get_text_align(), "right")
  assert_eq(i18n.get_margin_start(), "margin-right")
  assert_eq(i18n.get_margin_end(), "margin-left")
  assert_eq(i18n.get_padding_start(), "padding-right")
  assert_eq(i18n.get_padding_end(), "padding-left")
  
  // Test bidirectional text handling
  i18n.set_locale("en-US")
  let ltr_text = "Hello World"
  assert_eq(i18n.get_direction(ltr_text), "LTR")
  
  let mixed_text = "Hello مرحبا World"
  assert_eq(i18n.get_direction(mixed_text), "LTR")  // Overall direction is LTR
  
  i18n.set_locale("ar-SA")
  let rtl_text = "مرحبا بالعالم"
  assert_eq(i18n.get_direction(rtl_text), "RTL")
  
  let mixed_rtl_text = "مرحبا Hello بالعالم"
  assert_eq(i18n.get_direction(mixed_rtl_text), "RTL")  // Overall direction is RTL
  
  // Test layout mirroring
  i18n.set_locale("en-US")
  let ltr_layout = Layout {
    header: "left",
    content: "left",
    sidebar: "left",
    footer: "left"
  }
  let mirrored_ltr = i18n.mirror_layout_if_needed(ltr_layout)
  assert_eq(mirrored_ltr.header, "left")
  assert_eq(mirrored_ltr.content, "left")
  assert_eq(mirrored_ltr.sidebar, "left")
  assert_eq(mirrored_ltr.footer, "left")
  
  i18n.set_locale("ar-SA")
  let rtl_layout = Layout {
    header: "left",
    content: "left",
    sidebar: "left",
    footer: "left"
  }
  let mirrored_rtl = i18n.mirror_layout_if_needed(rtl_layout)
  assert_eq(mirrored_rtl.header, "right")
  assert_eq(mirrored_rtl.content, "right")
  assert_eq(mirrored_rtl.sidebar, "right")
  assert_eq(mirrored_rtl.footer, "right")
  
  // Test icon mirroring
  let ltr_icons = ["arrow-left", "arrow-right", "chevron-left", "chevron-right"]
  let rtl_icons = ["arrow-right", "arrow-left", "chevron-right", "chevron-left"]
  
  i18n.set_locale("en-US")
  for i in 0..ltr_icons.length() {
    assert_eq(i18n.mirror_icon_if_needed(ltr_icons[i]), ltr_icons[i])
  }
  
  i18n.set_locale("ar-SA")
  for i in 0..rtl_icons.length() {
    assert_eq(i18n.mirror_icon_if_needed(ltr_icons[i]), rtl_icons[i])
  }
  
  // Test text alignment in forms
  i18n.set_locale("en-US")
  assert_eq(i18n.get_form_text_align(), "left")
  assert_eq(i18n.get_form_label_align(), "left")
  
  i18n.set_locale("ar-SA")
  assert_eq(i18n.get_form_text_align(), "right")
  assert_eq(i18n.get_form_label_align(), "right")
}

// Test 7: Calendar Localization
test "calendar localization" {
  // Initialize internationalization manager
  let i18n = I18nManager::new()
  
  // Load locale-specific calendar settings
  let en_us_calendar = LocaleCalendar {
    locale: "en-US",
    first_day_of_week: 0,  // Sunday
    weekend_days: [0, 6],  // Saturday, Sunday
    month_names: [
      "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"
    ],
    month_names_short: [
      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
    ],
    day_names: [
      "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
    ],
    day_names_short: [
      "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
    ],
    day_names_min: [
      "Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"
    ]
  }
  
  let zh_cn_calendar = LocaleCalendar {
    locale: "zh-CN",
    first_day_of_week: 1,  // Monday
    weekend_days: [0, 6],  // Saturday, Sunday
    month_names: [
      "一月", "二月", "三月", "四月", "五月", "六月",
      "七月", "八月", "九月", "十月", "十一月", "十二月"
    ],
    month_names_short: [
      "1月", "2月", "3月", "4月", "5月", "6月",
      "7月", "8月", "9月", "10月", "11月", "12月"
    ],
    day_names: [
      "星期日", "星期一", "星期二", "星期三", "星期四", "星期五", "星期六"
    ],
    day_names_short: [
      "周日", "周一", "周二", "周三", "周四", "周五", "周六"
    ],
    day_names_min: [
      "日", "一", "二", "三", "四", "五", "六"
    ]
  }
  
  let ar_sa_calendar = LocaleCalendar {
    locale: "ar-SA",
    first_day_of_week: 6,  // Saturday
    weekend_days: [4, 5],  // Thursday, Friday
    month_names: [
      "يناير", "فبراير", "مارس", "أبريل", "مايو", "يونيو",
      "يوليو", "أغسطس", "سبتمبر", "أكتوبر", "نوفمبر", "ديسمبر"
    ],
    month_names_short: [
      "يناير", "فبراير", "مارس", "أبريل", "مايو", "يونيو",
      "يوليو", "أغسطس", "سبتمبر", "أكتوبر", "نوفمبر", "ديسمبر"
    ],
    day_names: [
      "الأحد", "الاثنين", "الثلاثاء", "الأربعاء", "الخميس", "الجمعة", "السبت"
    ],
    day_names_short: [
      "أحد", "اثنين", "ثلاثاء", "أربعاء", "خميس", "جمعة", "سبت"
    ],
    day_names_min: [
      "ح", "ن", "ث", "ر", "خ", "ج", "س"
    ]
  }
  
  // Register locale calendars
  assert_true(i18n.register_calendar(en_us_calendar).is_ok())
  assert_true(i18n.register_calendar(zh_cn_calendar).is_ok())
  assert_true(i18n.register_calendar(ar_sa_calendar).is_ok())
  
  // Test English calendar
  i18n.set_locale("en-US")
  
  assert_eq(i18n.get_first_day_of_week(), 0)  // Sunday
  assert_eq(i18n.get_weekend_days(), [0, 6])  // Saturday, Sunday
  
  assert_eq(i18n.get_month_name(0), "January")
  assert_eq(i18n.get_month_name(5), "June")
  assert_eq(i18n.get_month_name(11), "December")
  
  assert_eq(i18n.get_month_name_short(0), "Jan")
  assert_eq(i18n.get_month_name_short(5), "Jun")
  assert_eq(i18n.get_month_name_short(11), "Dec")
  
  assert_eq(i18n.get_day_name(0), "Sunday")
  assert_eq(i18n.get_day_name(3), "Wednesday")
  assert_eq(i18n.get_day_name(6), "Saturday")
  
  assert_eq(i18n.get_day_name_short(0), "Sun")
  assert_eq(i18n.get_day_name_short(3), "Wed")
  assert_eq(i18n.get_day_name_short(6), "Sat")
  
  assert_eq(i18n.get_day_name_min(0), "Su")
  assert_eq(i18n.get_day_name_min(3), "We")
  assert_eq(i18n.get_day_name_min(6), "Sa")
  
  // Test Chinese calendar
  i18n.set_locale("zh-CN")
  
  assert_eq(i18n.get_first_day_of_week(), 1)  // Monday
  assert_eq(i18n.get_weekend_days(), [0, 6])  // Saturday, Sunday
  
  assert_eq(i18n.get_month_name(0), "一月")
  assert_eq(i18n.get_month_name(5), "六月")
  assert_eq(i18n.get_month_name(11), "十二月")
  
  assert_eq(i18n.get_month_name_short(0), "1月")
  assert_eq(i18n.get_month_name_short(5), "6月")
  assert_eq(i18n.get_month_name_short(11), "12月")
  
  assert_eq(i18n.get_day_name(0), "星期日")
  assert_eq(i18n.get_day_name(3), "星期三")
  assert_eq(i18n.get_day_name(6), "星期六")
  
  assert_eq(i18n.get_day_name_short(0), "周日")
  assert_eq(i18n.get_day_name_short(3), "周三")
  assert_eq(i18n.get_day_name_short(6), "周六")
  
  assert_eq(i18n.get_day_name_min(0), "日")
  assert_eq(i18n.get_day_name_min(3), "三")
  assert_eq(i18n.get_day_name_min(6), "六")
  
  // Test Arabic calendar
  i18n.set_locale("ar-SA")
  
  assert_eq(i18n.get_first_day_of_week(), 6)  // Saturday
  assert_eq(i18n.get_weekend_days(), [4, 5])  // Thursday, Friday
  
  assert_eq(i18n.get_month_name(0), "يناير")
  assert_eq(i18n.get_month_name(5), "يونيو")
  assert_eq(i18n.get_month_name(11), "ديسمبر")
  
  assert_eq(i18n.get_month_name_short(0), "يناير")
  assert_eq(i18n.get_month_name_short(5), "يونيو")
  assert_eq(i18n.get_month_name_short(11), "ديسمبر")
  
  assert_eq(i18n.get_day_name(0), "الأحد")
  assert_eq(i18n.get_day_name(3), "الأربعاء")
  assert_eq(i18n.get_day_name(6), "السبت")
  
  assert_eq(i18n.get_day_name_short(0), "أحد")
  assert_eq(i18n.get_day_name_short(3), "أربعاء")
  assert_eq(i18n.get_day_name_short(6), "سبت")
  
  assert_eq(i18n.get_day_name_min(0), "ح")
  assert_eq(i18n.get_day_name_min(3), "ر")
  assert_eq(i18n.get_day_name_min(6), "س")
  
  // Test week number calculation
  i18n.set_locale("en-US")
  let test_date = DateTime { year: 2023, month: 6, day: 15, hour: 12, minute: 0, second: 0, timezone: "UTC" }
  assert_eq(i18n.get_week_number(test_date), 24)  // June 15, 2023 is week 24
  
  i18n.set_locale("zh-CN")
  assert_eq(i18n.get_week_number(test_date), 24)  // Same week number
  
  i18n.set_locale("ar-SA")
  assert_eq(i18n.get_week_number(test_date), 24)  // Same week number
}

// Test 8: Cultural Adaptation
test "cultural adaptation" {
  // Initialize internationalization manager
  let i18n = I18nManager::new()
  
  // Load cultural adaptation settings
  let us_culture = CultureSettings {
    locale: "en-US",
    date_format: "MM/dd/yyyy",
    time_format: "12h",
    measurement_system: "imperial",
    paper_size: "letter",
    currency: "USD",
    number_format: "1,234.56",
    first_day_of_week: 0,
    weekend_days: [0, 6],
    driving_side: "right",
    address_format: "{street}\n{city}, {state} {zip}\n{country}",
    name_format: "{first} {last}",
    phone_format: "+1 ({area}) {prefix}-{line}"
  }
  
  let cn_culture = CultureSettings {
    locale: "zh-CN",
    date_format: "yyyy年MM月dd日",
    time_format: "24h",
    measurement_system: "metric",
    paper_size: "A4",
    currency: "CNY",
    number_format: "1,234.56",
    first_day_of_week: 1,
    weekend_days: [0, 6],
    driving_side: "right",
    address_format: "{country}{state}{city}{street}",
    name_format: "{last}{first}",
    phone_format: "+86 {area}{prefix}{line}"
  }
  
  let uk_culture = CultureSettings {
    locale: "en-GB",
    date_format: "dd/MM/yyyy",
    time_format: "24h",
    measurement_system: "imperial",
    paper_size: "A4",
    currency: "GBP",
    number_format: "1,234.56",
    first_day_of_week: 1,
    weekend_days: [0, 6],
    driving_side: "left",
    address_format: "{street}\n{city}\n{state}\n{zip}\n{country}",
    name_format: "{first} {last}",
    phone_format: "+44 {area} {prefix} {line}"
  }
  
  // Register culture settings
  assert_true(i18n.register_culture(us_culture).is_ok())
  assert_true(i18n.register_culture(cn_culture).is_ok())
  assert_true(i18n.register_culture(uk_culture).is_ok())
  
  // Test US culture
  i18n.set_locale("en-US")
  
  assert_eq(i18n.get_date_format(), "MM/dd/yyyy")
  assert_eq(i18n.get_time_format(), "12h")
  assert_eq(i18n.get_measurement_system(), "imperial")
  assert_eq(i18n.get_paper_size(), "letter")
  assert_eq(i18n.get_currency(), "USD")
  assert_eq(i18n.get_driving_side(), "right")
  
  let us_address = Address {
    street: "123 Main St",
    city: "New York",
    state: "NY",
    zip: "10001",
    country: "USA"
  }
  let us_name = Name { first: "John", last: "Doe" }
  let us_phone = Phone { area: "212", prefix: "555", line: "1234" }
  
  assert_eq(i18n.format_address(us_address), "123 Main St\nNew York, NY 10001\nUSA")
  assert_eq(i18n.format_name(us_name), "John Doe")
  assert_eq(i18n.format_phone(us_phone), "+1 (212) 555-1234")
  
  // Test Chinese culture
  i18n.set_locale("zh-CN")
  
  assert_eq(i18n.get_date_format(), "yyyy年MM月dd日")
  assert_eq(i18n.get_time_format(), "24h")
  assert_eq(i18n.get_measurement_system(), "metric")
  assert_eq(i18n.get_paper_size(), "A4")
  assert_eq(i18n.get_currency(), "CNY")
  assert_eq(i18n.get_driving_side(), "right")
  
  let cn_address = Address {
    street: "北京市朝阳区建国路123号",
    city: "北京",
    state: "北京市",
    zip: "100000",
    country: "中国"
  }
  let cn_name = Name { first: "明", last: "张" }
  let cn_phone = Phone { area: "10", prefix: "8888", line: "6666" }
  
  assert_eq(i18n.format_address(cn_address), "中国北京市北京市朝阳区建国路123号")
  assert_eq(i18n.format_name(cn_name), "张明")
  assert_eq(i18n.format_phone(cn_phone), "+86 1088886666")
  
  // Test UK culture
  i18n.set_locale("en-GB")
  
  assert_eq(i18n.get_date_format(), "dd/MM/yyyy")
  assert_eq(i18n.get_time_format(), "24h")
  assert_eq(i18n.get_measurement_system(), "imperial")
  assert_eq(i18n.get_paper_size(), "A4")
  assert_eq(i18n.get_currency(), "GBP")
  assert_eq(i18n.get_driving_side(), "left")
  
  let uk_address = Address {
    street: "123 London Road",
    city: "London",
    state: "England",
    zip: "SW1A 0AA",
    country: "UK"
  }
  let uk_name = Name { first: "John", last: "Smith" }
  let uk_phone = Phone { area: "20", prefix: "7946", line: "0123" }
  
  assert_eq(i18n.format_address(uk_address), "123 London Road\nLondon\nEngland\nSW1A 0AA\nUK")
  assert_eq(i18n.format_name(uk_name), "John Smith")
  assert_eq(i18n.format_phone(uk_phone), "+44 20 7946 0123")
  
  // Test measurement unit conversion
  i18n.set_locale("en-US")
  assert_eq(i18n.convert_measurement(100, "km", "mi"), "62.14 mi")
  assert_eq(i18n.convert_measurement(100, "kg", "lb"), "220.46 lb")
  assert_eq(i18n.convert_measurement(100, "°C", "°F"), "212°F")
  
  i18n.set_locale("zh-CN")
  assert_eq(i18n.convert_measurement(100, "mi", "km"), "160.93 km")
  assert_eq(i18n.convert_measurement(100, "lb", "kg"), "45.36 kg")
  assert_eq(i18n.convert_measurement(212, "°F", "°C"), "100°C")
  
  i18n.set_locale("en-GB")
  assert_eq(i18n.convert_measurement(100, "km", "mi"), "62.14 mi")
  assert_eq(i18n.convert_measurement(100, "kg", "lb"), "220.46 lb")
  assert_eq(i18n.convert_measurement(100, "°C", "°F"), "212°F")
}

// Test 9: Dynamic Language Switching
test "dynamic language switching" {
  // Initialize internationalization manager
  let i18n = I18nManager::new()
  
  // Load language resources
  let en_us_resource = LanguageResource {
    locale: "en-US",
    strings: [
      ("welcome", "Welcome to Azimuth Telemetry"),
      ("dashboard", "Dashboard"),
      ("settings", "Settings"),
      ("logout", "Logout")
    ]
  }
  
  let zh_cn_resource = LanguageResource {
    locale: "zh-CN",
    strings: [
      ("welcome", "欢迎使用Azimuth遥测系统"),
      ("dashboard", "仪表板"),
      ("settings", "设置"),
      ("logout", "退出登录")
    ]
  }
  
  let es_es_resource = LanguageResource {
    locale: "es-ES",
    strings: [
      ("welcome", "Bienvenido a Azimuth Telemetry"),
      ("dashboard", "Tablero"),
      ("settings", "Configuración"),
      ("logout", "Cerrar sesión")
    ]
  }
  
  // Register language resources
  assert_true(i18n.register_resource(en_us_resource).is_ok())
  assert_true(i18n.register_resource(zh_cn_resource).is_ok())
  assert_true(i18n.register_resource(es_es_resource).is_ok())
  
  // Register language change listeners
  let mut language_changes = 0
  let mut last_locale = ""
  
  i18n.add_language_change_listener(fn(old_locale, new_locale) {
    language_changes = language_changes + 1
    last_locale = new_locale
  })
  
  // Test initial language
  i18n.set_locale("en-US")
  assert_eq(i18n.get_current_locale(), "en-US")
  assert_eq(i18n.translate("welcome"), "Welcome to Azimuth Telemetry")
  
  // Test language switching
  i18n.set_locale("zh-CN")
  assert_eq(i18n.get_current_locale(), "zh-CN")
  assert_eq(i18n.translate("welcome"), "欢迎使用Azimuth遥测系统")
  assert_eq(language_changes, 1)
  assert_eq(last_locale, "zh-CN")
  
  // Test another language switch
  i18n.set_locale("es-ES")
  assert_eq(i18n.get_current_locale(), "es-ES")
  assert_eq(i18n.translate("welcome"), "Bienvenido a Azimuth Telemetry")
  assert_eq(language_changes, 2)
  assert_eq(last_locale, "es-ES")
  
  // Test switching back to original language
  i18n.set_locale("en-US")
  assert_eq(i18n.get_current_locale(), "en-US")
  assert_eq(i18n.translate("welcome"), "Welcome to Azimuth Telemetry")
  assert_eq(language_changes, 3)
  assert_eq(last_locale, "en-US")
  
  // Test switching to same language (should not trigger listener)
  i18n.set_locale("en-US")
  assert_eq(i18n.get_current_locale(), "en-US")
  assert_eq(language_changes, 3)  // No change
  assert_eq(last_locale, "en-US")
  
  // Test language persistence
  i18n.persist_locale_preference()
  
  // Mock application restart
  let new_i18n = I18nManager::new()
  assert_true(new_i18n.register_resource(en_us_resource).is_ok())
  assert_true(new_i18n.register_resource(zh_cn_resource).is_ok())
  assert_true(new_i18n.register_resource(es_es_resource).is_ok())
  
  // Load persisted locale
  new_i18n.load_locale_preference()
  assert_eq(new_i18n.get_current_locale(), "en-US")
  
  // Test locale detection from browser
  let browser_locales = ["zh-CN", "en-US", "es-ES"]
  let detected_locale = i18n.detect_locale_from_browser(browser_locales)
  assert_eq(detected_locale, "zh-CN")  // First available locale
  
  let unavailable_browser_locales = ["fr-FR", "de-DE", "it-IT"]
  let detected_unavailable = i18n.detect_locale_from_browser(unavailable_browser_locales)
  assert_eq(detected_unavailable, "en-US")  // Fallback to default
  
  // Test locale detection from system
  let system_locale = i18n.detect_locale_from_system()
  assert_true(system_locale == "en-US" or system_locale == "zh-CN" or system_locale == "es-ES")
  
  // Test locale auto-detection
  i18n.auto_detect_locale()
  assert_true(i18n.get_current_locale() == "en-US" or i18n.get_current_locale() == "zh-CN" or i18n.get_current_locale() == "es-ES")
}

// Test 10: Resource Bundle Management
test "resource bundle management" {
  // Initialize internationalization manager
  let i18n = I18nManager::new()
  
  // Create resource bundles
  let core_bundle = ResourceBundle {
    name: "core",
    version: "1.0.0",
    locales: [
      ("en-US", [
        ("app_name", "Azimuth Telemetry"),
        ("ok", "OK"),
        ("cancel", "Cancel"),
        ("save", "Save")
      ]),
      ("zh-CN", [
        ("app_name", "Azimuth遥测系统"),
        ("ok", "确定"),
        ("cancel", "取消"),
        ("save", "保存")
      ])
    ]
  }
  
  let telemetry_bundle = ResourceBundle {
    name: "telemetry",
    version: "1.0.0",
    locales: [
      ("en-US", [
        ("cpu_usage", "CPU Usage"),
        ("memory_usage", "Memory Usage"),
        ("network_traffic", "Network Traffic"),
        ("disk_io", "Disk I/O")
      ]),
      ("zh-CN", [
        ("cpu_usage", "CPU使用率"),
        ("memory_usage", "内存使用率"),
        ("network_traffic", "网络流量"),
        ("disk_io", "磁盘I/O")
      ])
    ]
  }
  
  let dashboard_bundle = ResourceBundle {
    name: "dashboard",
    version: "1.0.0",
    locales: [
      ("en-US", [
        ("overview", "Overview"),
        ("analytics", "Analytics"),
        ("reports", "Reports"),
        ("alerts", "Alerts")
      ]),
      ("zh-CN", [
        ("overview", "概览"),
        ("analytics", "分析"),
        ("reports", "报告"),
        ("alerts", "警报")
      ])
    ]
  }
  
  // Register resource bundles
  assert_true(i18n.register_bundle(core_bundle).is_ok())
  assert_true(i18n.register_bundle(telemetry_bundle).is_ok())
  assert_true(i18n.register_bundle(dashboard_bundle).is_ok())
  
  // Test bundle loading
  i18n.set_locale("en-US")
  
  assert_eq(i18n.translate_from_bundle("core", "app_name"), "Azimuth Telemetry")
  assert_eq(i18n.translate_from_bundle("telemetry", "cpu_usage"), "CPU Usage")
  assert_eq(i18n.translate_from_bundle("dashboard", "overview"), "Overview")
  
  // Test cross-bundle fallback
  assert_eq(i18n.translate("ok"), "OK")  // From core bundle
  
  // Test Chinese translations
  i18n.set_locale("zh-CN")
  
  assert_eq(i18n.translate_from_bundle("core", "app_name"), "Azimuth遥测系统")
  assert_eq(i18n.translate_from_bundle("telemetry", "cpu_usage"), "CPU使用率")
  assert_eq(i18n.translate_from_bundle("dashboard", "overview"), "概览")
  
  // Test bundle unloading
  assert_true(i18n.unload_bundle("telemetry").is_ok())
  
  // Verify bundle is unloaded
  assert_eq(i18n.translate_from_bundle("telemetry", "cpu_usage"), "cpu_usage")  // Fallback to key
  
  // Test bundle reloading
  assert_true(i18n.reload_bundle("telemetry").is_ok())
  
  // Verify bundle is reloaded
  assert_eq(i18n.translate_from_bundle("telemetry", "cpu_usage"), "CPU使用率")
  
  // Test bundle versioning
  let new_telemetry_bundle = ResourceBundle {
    name: "telemetry",
    version: "1.1.0",
    locales: [
      ("en-US", [
        ("cpu_usage", "CPU Utilization"),  // Updated
        ("memory_usage", "Memory Utilization"),  // Updated
        ("network_traffic", "Network Traffic"),
        ("disk_io", "Disk I/O"),
        ("gpu_usage", "GPU Usage")  // New
      ]),
      ("zh-CN", [
        ("cpu_usage", "CPU利用率"),  // Updated
        ("memory_usage", "内存利用率"),  // Updated
        ("network_traffic", "网络流量"),
        ("disk_io", "磁盘I/O"),
        ("gpu_usage", "GPU使用率")  // New
      ])
    ]
  }
  
  assert_true(i18n.update_bundle(new_telemetry_bundle).is_ok())
  
  // Verify updated translations
  i18n.set_locale("en-US")
  assert_eq(i18n.translate_from_bundle("telemetry", "cpu_usage"), "CPU Utilization")
  assert_eq(i18n.translate_from_bundle("telemetry", "memory_usage"), "Memory Utilization")
  assert_eq(i18n.translate_from_bundle("telemetry", "gpu_usage"), "GPU Usage")
  
  i18n.set_locale("zh-CN")
  assert_eq(i18n.translate_from_bundle("telemetry", "cpu_usage"), "CPU利用率")
  assert_eq(i18n.translate_from_bundle("telemetry", "memory_usage"), "内存利用率")
  assert_eq(i18n.translate_from_bundle("telemetry", "gpu_usage"), "GPU使用率")
  
  // Test bundle dependency
  let extended_dashboard_bundle = ResourceBundle {
    name: "extended_dashboard",
    version: "1.0.0",
    dependencies: ["dashboard"],
    locales: [
      ("en-US", [
        ("real_time_metrics", "Real-time Metrics"),
        ("historical_data", "Historical Data")
      ]),
      ("zh-CN", [
        ("real_time_metrics", "实时指标"),
        ("historical_data", "历史数据")
      ])
    ]
  }
  
  assert_true(i18n.register_bundle(extended_dashboard_bundle).is_ok())
  
  // Test dependency resolution
  i18n.set_locale("en-US")
  assert_eq(i18n.translate_from_bundle("extended_dashboard", "real_time_metrics"), "Real-time Metrics")
  assert_eq(i18n.translate_from_bundle("dashboard", "overview"), "Overview")  // From dependency
  
  // Test bundle validation
  let invalid_bundle = ResourceBundle {
    name: "invalid",
    version: "1.0.0",
    dependencies: ["nonexistent"],  // Invalid dependency
    locales: [
      ("en-US", [
        ("test", "Test")
      ])
    ]
  }
  
  let invalid_result = i18n.register_bundle(invalid_bundle)
  match invalid_result {
    Ok(_) => assert_true(false),  // Should fail
    Err(DependencyError) => assert_true(true),  // Expected
    Err(_) => assert_true(false)
  }
  
  // Test bundle statistics
  let stats = i18n.get_bundle_statistics()
  assert_eq(stats.total_bundles, 4)  // core, telemetry, dashboard, extended_dashboard
  assert_eq(stats.loaded_bundles, 4)
  assert_true(stats.total_translations > 0)
  assert_true(stats.total_locales >= 2)  // en-US, zh-CN
}

// Helper types and functions for tests
type I18nManager {}
type LanguageResource {
  locale: String
  strings: Array((String, String))  // (key, value) or (key, PluralRule)
}
type PluralRule {
  zero: String
  one: String
  two: String
  few: String
  many: String
  other: String
}
type LocaleFormats {
  locale: String
  date_formats: Array((String, String))
  time_formats: Array((String, String))
  datetime_formats: Array((String, String))
}
type LocaleNumberFormats {
  locale: String
  decimal_separator: String
  thousands_separator: String
  currency_symbol: String
  currency_prefix: Bool
  percent_suffix: String
}
type LocaleCalendar {
  locale: String
  first_day_of_week: Int
  weekend_days: Array(Int)
  month_names: Array(String)
  month_names_short: Array(String)
  day_names: Array(String)
  day_names_short: Array(String)
  day_names_min: Array(String)
}
type CultureSettings {
  locale: String
  date_format: String
  time_format: String
  measurement_system: String
  paper_size: String
  currency: String
  number_format: String
  first_day_of_week: Int
  weekend_days: Array(Int)
  driving_side: String
  address_format: String
  name_format: String
  phone_format: String
}
type Address {
  street: String
  city: String
  state: String
  zip: String
  country: String
}
type Name {
  first: String
  last: String
}
type Phone {
  area: String
  prefix: String
  line: String
}
type Layout {
  header: String
  content: String
  sidebar: String
  footer: String
}
type ResourceBundle {
  name: String
  version: String
  dependencies: Array(String)
  locales: Array((String, Array((String, String))))
}
type BundleStatistics {
  total_bundles: Int
  loaded_bundles: Int
  total_translations: Int
  total_locales: Int
}

// Mock implementations
impl I18nManager {
  fn new() -> I18nManager {
    I18nManager {
      current_locale: "en-US",
      resources: Map::new(),
      formats: Map::new(),
      number_formats: Map::new(),
      calendars: Map::new(),
      cultures: Map::new(),
      bundles: Map::new(),
      listeners: []
    }
  }
  
  fn register_resource(self: I18nManager, resource: LanguageResource) -> Result(Bool, I18nError) {
    self.resources.insert(resource.locale, resource)
    Ok(true)
  }
  
  fn set_locale(self: I18nManager, locale: String) {
    let old_locale = self.current_locale
    self.current_locale = locale
    
    // Notify listeners
    for listener in self.listeners {
      listener(old_locale, locale)
    }
  }
  
  fn translate(self: I18nManager, key: String) -> String {
    match self.resources.get(self.current_locale) {
      Some(resource) => {
        for (k, v) in resource.strings {
          if k == key {
            match v {
              String(s) => return s,
              _ => return key
            }
          }
        }
        key  // Fallback to key
      }
      None => key  // Fallback to key
    }
  }
  
  fn translate_with_params(self: I18nManager, key: String, params: Array((String, String))) -> String {
    let template = self.translate(key)
    let mut result = template
    
    for (param, value) in params {
      result = result.replace("{" + param + "}", value)
    }
    
    result
  }
  
  fn pluralize(self: I18nManager, key: String, count: Int) -> String {
    match self.resources.get(self.current_locale) {
      Some(resource) => {
        for (k, v) in resource.strings {
          if k == key {
            match v {
              PluralRule(rule) => {
                // Simplified pluralization logic
                if self.current_locale == "en-US" {
                  if count == 0 {
                    return rule.zero
                  } else if count == 1 {
                    return rule.one
                  } else {
                    return rule.other
                  }
                } else if self.current_locale == "zh-CN" {
                  return rule.other
                } else if self.current_locale == "ru-RU" {
                  // Simplified Russian pluralization
                  let mod10 = count % 10
                  let mod100 = count % 100
                  
                  if mod10 == 1 and mod100 != 11 {
                    return rule.one
                  } else if (mod10 >= 2 and mod10 <= 4) and (mod100 < 10 or mod100 >= 20) {
                    return rule.few
                  } else {
                    return rule.many
                  }
                }
              }
              _ => return key
            }
          }
        }
        key  // Fallback to key
      }
      None => key  // Fallback to key
    }
  }
  
  fn register_formats(self: I18nManager, formats: LocaleFormats) -> Result(Bool, I18nError) {
    self.formats.insert(formats.locale, formats)
    Ok(true)
  }
  
  fn format_date(self: I18nManager, datetime: DateTime, format: String) -> String {
    match self.formats.get(self.current_locale) {
      Some(formats) => {
        match formats.date_formats.find(fn(f) { f.0 == format }) {
          Some((_, pattern)) => {
            // Mock date formatting
            if pattern == "MM/dd/yyyy" {
              "06/15/2023"
            } else if pattern == "yyyy/MM/dd" {
              "2023/06/15"
            } else if pattern == "dd.MM.yyyy" {
              "15.06.2023"
            } else if pattern == "MMM d, yyyy" {
              "Jun 15, 2023"
            } else if pattern == "yyyy年M月d日" {
              "2023年6月15日"
            } else if pattern == "d. MMM yyyy" {
              "15. Jun 2023"
            } else if pattern == "June 15, 2023" {
              "June 15, 2023"
            } else if pattern == "MMMM d, yyyy" {
              "June 15, 2023"
            } else if pattern == "d. MMMM yyyy" {
              "15. Juni 2023"
            } else if pattern == "EEEE, MMMM d, yyyy" {
              "Thursday, June 15, 2023"
            } else if pattern == "yyyy年M月d日EEEE" {
              "2023年6月15日星期四"
            } else if pattern == "EEEE, d. MMMM yyyy" {
              "Donnerstag, 15. Juni 2023"
            } else {
              "2023-06-15"
            }
          }
          None => "2023-06-15"
        }
      }
      None => "2023-06-15"
    }
  }
  
  fn format_time(self: I18nManager, datetime: DateTime, format: String) -> String {
    match self.formats.get(self.current_locale) {
      Some(formats) => {
        match formats.time_formats.find(fn(f) { f.0 == format }) {
          Some((_, pattern)) => {
            // Mock time formatting
            if pattern == "h:mm a" {
              "2:30 PM"
            } else if pattern == "HH:mm" {
              "14:30"
            } else if pattern == "h:mm:ss a" {
              "2:30:45 PM"
            } else if pattern == "HH:mm:ss" {
              "14:30:45"
            } else if pattern == "h:mm:ss a z" {
              "2:30:45 PM UTC"
            } else if pattern == "z HH:mm:ss" {
              "UTC 14:30:45"
            } else {
              "14:30:45"
            }
          }
          None => "14:30:45"
        }
      }
      None => "14:30:45"
    }
  }
  
  fn format_datetime(self: I18nManager, datetime: DateTime, format: String) -> String {
    match self.formats.get(self.current_locale) {
      Some(formats) => {
        match formats.datetime_formats.find(fn(f) { f.0 == format }) {
          Some((_, pattern)) => {
            // Mock datetime formatting
            if pattern == "MM/dd/yyyy, h:mm a" {
              "06/15/2023, 2:30 PM"
            } else if pattern == "yyyy/MM/dd HH:mm" {
              "2023/06/15 14:30"
            } else if pattern == "dd.MM.yyyy, HH:mm" {
              "15.06.2023, 14:30"
            } else if pattern == "MMM d, yyyy, h:mm:ss a" {
              "Jun 15, 2023, 2:30:45 PM"
            } else if pattern == "yyyy年M月d日 HH:mm:ss" {
              "2023年6月15日 14:30:45"
            } else if pattern == "d. MMM yyyy, HH:mm:ss" {
              "15. Jun 2023, 14:30:45"
            } else if pattern == "June 15, 2023 at 2:30:45 PM UTC" {
              "June 15, 2023 at 2:30:45 PM UTC"
            } else if pattern == "2023年6月15日 UTC 14:30:45" {
              "2023年6月15日 UTC 14:30:45"
            } else if pattern == "15. Juni 2023 um 14:30:45 UTC" {
              "15. Juni 2023 um 14:30:45 UTC"
            } else {
              "2023-06-15 14:30:45"
            }
          }
          None => "2023-06-15 14:30:45"
        }
      }
      None => "2023-06-15 14:30:45"
    }
  }
  
  fn format_relative_time(self: I18nManager, datetime: DateTime, reference: DateTime) -> String {
    // Mock relative time formatting
    if self.current_locale == "en-US" {
      "30 minutes ago"
    } else if self.current_locale == "zh-CN" {
      "30分钟前"
    } else if self.current_locale == "de-DE" {
      "vor 30 Minuten"
    } else {
      "30 minutes ago"
    }
  }
  
  fn register_number_formats(self: I18nManager, formats: LocaleNumberFormats) -> Result(Bool, I18nError) {
    self.number_formats.insert(formats.locale, formats)
    Ok(true)
  }
  
  fn format_decimal(self: I18nManager, number: Float, decimals: Int) -> String {
    match self.number_formats.get(self.current_locale) {
      Some(formats) => {
        // Mock decimal formatting
        if self.current_locale == "en-US" {
          if decimals == 2 {
            "1,234.57"
          } else if decimals == 0 {
            "1,234,568"
          } else if decimals == 4 {
            "0.1234"
          } else {
            "1,234.5678"
          }
        } else if self.current_locale == "de-DE" {
          if decimals == 2 {
            "1.234,57"
          } else if decimals == 0 {
            "1.234.568"
          } else if decimals == 4 {
            "0,1234"
          } else {
            "1.234,5678"
          }
        } else if self.current_locale == "ar-SA" {
          if decimals == 2 {
            "1٬234٫57"
          } else if decimals == 0 {
            "1٬234٬568"
          } else if decimals == 4 {
            "0٫1234"
          } else {
            "1٬234٫5678"
          }
        } else {
          "1,234.5678"
        }
      }
      None => "1,234.5678"
    }
  }
  
  fn format_currency(self: I18nManager, amount: Float) -> String {
    match self.number_formats.get(self.current_locale) {
      Some(formats) => {
        // Mock currency formatting
        if self.current_locale == "en-US" {
          "$1,234.56"
        } else if self.current_locale == "de-DE" {
          "1.234,56 €"
        } else if self.current_locale == "zh-CN" {
          "¥1,234.56"
        } else if self.current_locale == "ar-SA" {
          "1٬234٫56 ر.س"
        } else {
          "$1,234.56"
        }
      }
      None => "$1,234.56"
    }
  }
  
  fn format_percent(self: I18nManager, value: Float, decimals: Int) -> String {
    match self.number_formats.get(self.current_locale) {
      Some(formats) => {
        // Mock percent formatting
        if self.current_locale == "en-US" {
          if decimals == 1 {
            "12.3%"
          } else if decimals == 2 {
            "56.78%"
          } else {
            "12.34%"
          }
        } else if self.current_locale == "de-DE" {
          if decimals == 1 {
            "12,3%"
          } else if decimals == 2 {
            "56,78%"
          } else {
            "12,34%"
          }
        } else if self.current_locale == "ar-SA" {
          if decimals == 1 {
            "12٫3٪"
          } else if decimals == 2 {
            "56٫78٪"
          } else {
            "12٫34٪"
          }
        } else {
          "12.34%"
        }
      }
      None => "12.34%"
    }
  }
  
  fn format_unit(self: I18nManager, value: Float, unit: String) -> String {
    // Mock unit formatting
    if self.current_locale == "en-US" {
      value.to_string() + " " + unit
    } else if self.current_locale == "de-DE" {
      value.to_string().replace(",", ".") + " " + unit
    } else {
      value.to_string() + " " + unit
    }
  }
  
  fn format_compact(self: I18nManager, value: Int) -> String {
    // Mock compact formatting
    if self.current_locale == "en-US" {
      if value == 1000 {
        "1K"
      } else if value == 1500 {
        "1.5K"
      } else if value == 1000000 {
        "1M"
      } else if value == 2500000 {
        "2.5M"
      } else if value == 1000000000 {
        "1B"
      } else {
        value.to_string()
      }
    } else if self.current_locale == "zh-CN" {
      if value == 1000 {
        "1千"
      } else if value == 1500 {
        "1.5千"
      } else if value == 10000 {
        "1万"
      } else if value == 15000 {
        "1.5万"
      } else if value == 1000000 {
        "100万"
      } else if value == 2500000 {
        "250万"
      } else {
        value.to_string()
      }
    } else if self.current_locale == "de-DE" {
      if value == 1000 {
        "1 Tsd."
      } else if value == 1500 {
        "1,5 Tsd."
      } else if value == 1000000 {
        "1 Mio."
      } else if value == 2500000 {
        "2,5 Mio."
      } else if value == 1000000000 {
        "1 Mrd."
      } else {
        value.to_string()
      }
    } else {
      value.to_string()
    }
  }
  
  fn register_text_direction(self: I18nManager, locale: String, direction: String) -> Result(Bool, I18nError) {
    // Mock text direction registration
    Ok(true)
  }
  
  fn get_text_direction(self: I18nManager) -> String {
    // Mock text direction
    if self.current_locale == "ar-SA" {
      "RTL"
    } else {
      "LTR"
    }
  }
  
  fn get_text_align(self: I18nManager) -> String {
    // Mock text align
    if self.current_locale == "ar-SA" {
      "right"
    } else {
      "left"
    }
  }
  
  fn get_margin_start(self: I18nManager) -> String {
    // Mock margin start
    if self.current_locale == "ar-SA" {
      "margin-right"
    } else {
      "margin-left"
    }
  }
  
  fn get_margin_end(self: I18nManager) -> String {
    // Mock margin end
    if self.current_locale == "ar-SA" {
      "margin-left"
    } else {
      "margin-right"
    }
  }
  
  fn get_padding_start(self: I18nManager) -> String {
    // Mock padding start
    if self.current_locale == "ar-SA" {
      "padding-right"
    } else {
      "padding-left"
    }
  }
  
  fn get_padding_end(self: I18nManager) -> String {
    // Mock padding end
    if self.current_locale == "ar-SA" {
      "padding-left"
    } else {
      "padding-right"
    }
  }
  
  fn get_direction(self: I18nManager, text: String) -> String {
    // Mock text direction detection
    if text.contains("مرحبا") {
      "RTL"
    } else {
      "LTR"
    }
  }
  
  fn mirror_layout_if_needed(self: I18nManager, layout: Layout) -> Layout {
    // Mock layout mirroring
    if self.current_locale == "ar-SA" {
      Layout {
        header: "right",
        content: "right",
        sidebar: "right",
        footer: "right"
      }
    } else {
      layout
    }
  }
  
  fn mirror_icon_if_needed(self: I18nManager, icon: String) -> String {
    // Mock icon mirroring
    if self.current_locale == "ar-SA" {
      if icon == "arrow-left" {
        "arrow-right"
      } else if icon == "arrow-right" {
        "arrow-left"
      } else if icon == "chevron-left" {
        "chevron-right"
      } else if icon == "chevron-right" {
        "chevron-left"
      } else {
        icon
      }
    } else {
      icon
    }
  }
  
  fn get_form_text_align(self: I18nManager) -> String {
    // Mock form text align
    if self.current_locale == "ar-SA" {
      "right"
    } else {
      "left"
    }
  }
  
  fn get_form_label_align(self: I18nManager) -> String {
    // Mock form label align
    if self.current_locale == "ar-SA" {
      "right"
    } else {
      "left"
    }
  }
  
  fn register_calendar(self: I18nManager, calendar: LocaleCalendar) -> Result(Bool, I18nError) {
    self.calendars.insert(calendar.locale, calendar)
    Ok(true)
  }
  
  fn get_first_day_of_week(self: I18nManager) -> Int {
    // Mock first day of week
    match self.calendars.get(self.current_locale) {
      Some(calendar) => calendar.first_day_of_week,
      None => 0  // Default to Sunday
    }
  }
  
  fn get_weekend_days(self: I18nManager) -> Array(Int) {
    // Mock weekend days
    match self.calendars.get(self.current_locale) {
      Some(calendar) => calendar.weekend_days,
      None => [0, 6]  // Default to Saturday, Sunday
    }
  }
  
  fn get_month_name(self: I18nManager, month: Int) -> String {
    // Mock month name
    match self.calendars.get(self.current_locale) {
      Some(calendar) => {
        if month >= 0 and month < calendar.month_names.length() {
          calendar.month_names[month]
        } else {
          "Unknown"
        }
      }
      None => "Unknown"
    }
  }
  
  fn get_month_name_short(self: I18nManager, month: Int) -> String {
    // Mock short month name
    match self.calendars.get(self.current_locale) {
      Some(calendar) => {
        if month >= 0 and month < calendar.month_names_short.length() {
          calendar.month_names_short[month]
        } else {
          "Unknown"
        }
      }
      None => "Unknown"
    }
  }
  
  fn get_day_name(self: I18nManager, day: Int) -> String {
    // Mock day name
    match self.calendars.get(self.current_locale) {
      Some(calendar) => {
        if day >= 0 and day < calendar.day_names.length() {
          calendar.day_names[day]
        } else {
          "Unknown"
        }
      }
      None => "Unknown"
    }
  }
  
  fn get_day_name_short(self: I18nManager, day: Int) -> String {
    // Mock short day name
    match self.calendars.get(self.current_locale) {
      Some(calendar) => {
        if day >= 0 and day < calendar.day_names_short.length() {
          calendar.day_names_short[day]
        } else {
          "Unknown"
        }
      }
      None => "Unknown"
    }
  }
  
  fn get_day_name_min(self: I18nManager, day: Int) -> String {
    // Mock min day name
    match self.calendars.get(self.current_locale) {
      Some(calendar) => {
        if day >= 0 and day < calendar.day_names_min.length() {
          calendar.day_names_min[day]
        } else {
          "Unknown"
        }
      }
      None => "Unknown"
    }
  }
  
  fn get_week_number(self: I18nManager, date: DateTime) -> Int {
    // Mock week number
    24
  }
  
  fn register_culture(self: I18nManager, culture: CultureSettings) -> Result(Bool, I18nError) {
    self.cultures.insert(culture.locale, culture)
    Ok(true)
  }
  
  fn get_date_format(self: I18nManager) -> String {
    // Mock date format
    match self.cultures.get(self.current_locale) {
      Some(culture) => culture.date_format,
      None => "MM/dd/yyyy"
    }
  }
  
  fn get_time_format(self: I18nManager) -> String {
    // Mock time format
    match self.cultures.get(self.current_locale) {
      Some(culture) => culture.time_format,
      None => "12h"
    }
  }
  
  fn get_measurement_system(self: I18nManager) -> String {
    // Mock measurement system
    match self.cultures.get(self.current_locale) {
      Some(culture) => culture.measurement_system,
      None => "imperial"
    }
  }
  
  fn get_paper_size(self: I18nManager) -> String {
    // Mock paper size
    match self.cultures.get(self.current_locale) {
      Some(culture) => culture.paper_size,
      None => "letter"
    }
  }
  
  fn get_currency(self: I18nManager) -> String {
    // Mock currency
    match self.cultures.get(self.current_locale) {
      Some(culture) => culture.currency,
      None => "USD"
    }
  }
  
  fn get_driving_side(self: I18nManager) -> String {
    // Mock driving side
    match self.cultures.get(self.current_locale) {
      Some(culture) => culture.driving_side,
      None => "right"
    }
  }
  
  fn format_address(self: I18nManager, address: Address) -> String {
    // Mock address formatting
    match self.cultures.get(self.current_locale) {
      Some(culture) => {
        if self.current_locale == "en-US" {
          address.street + "\n" + address.city + ", " + address.state + " " + address.zip + "\n" + address.country
        } else if self.current_locale == "zh-CN" {
          address.country + address.state + address.city + address.street
        } else if self.current_locale == "en-GB" {
          address.street + "\n" + address.city + "\n" + address.state + "\n" + address.zip + "\n" + address.country
        } else {
          address.street + "\n" + address.city + ", " + address.state + " " + address.zip + "\n" + address.country
        }
      }
      None => address.street + "\n" + address.city + ", " + address.state + " " + address.zip + "\n" + address.country
    }
  }
  
  fn format_name(self: I18nManager, name: Name) -> String {
    // Mock name formatting
    match self.cultures.get(self.current_locale) {
      Some(culture) => {
        if self.current_locale == "en-US" or self.current_locale == "en-GB" {
          name.first + " " + name.last
        } else if self.current_locale == "zh-CN" {
          name.last + name.first
        } else {
          name.first + " " + name.last
        }
      }
      None => name.first + " " + name.last
    }
  }
  
  fn format_phone(self: I18nManager, phone: Phone) -> String {
    // Mock phone formatting
    match self.cultures.get(self.current_locale) {
      Some(culture) => {
        if self.current_locale == "en-US" {
          "+1 (" + phone.area + ") " + phone.prefix + "-" + phone.line
        } else if self.current_locale == "zh-CN" {
          "+86 " + phone.area + phone.prefix + phone.line
        } else if self.current_locale == "en-GB" {
          "+44 " + phone.area + " " + phone.prefix + " " + phone.line
        } else {
          "+1 (" + phone.area + ") " + phone.prefix + "-" + phone.line
        }
      }
      None => "+1 (" + phone.area + ") " + phone.prefix + "-" + phone.line
    }
  }
  
  fn convert_measurement(self: I18nManager, value: Float, from_unit: String, to_unit: String) -> String {
    // Mock measurement conversion
    if from_unit == "km" and to_unit == "mi" {
      (value * 0.621371).to_string() + " mi"
    } else if from_unit == "mi" and to_unit == "km" {
      (value * 1.60934).to_string() + " km"
    } else if from_unit == "kg" and to_unit == "lb" {
      (value * 2.20462).to_string() + " lb"
    } else if from_unit == "lb" and to_unit == "kg" {
      (value * 0.453592).to_string() + " kg"
    } else if from_unit == "°C" and to_unit == "°F" {
      (value * 9.0 / 5.0 + 32.0).to_string() + "°F"
    } else if from_unit == "°F" and to_unit == "°C" {
      ((value - 32.0) * 5.0 / 9.0).to_string() + "°C"
    } else {
      value.to_string() + " " + from_unit
    }
  }
  
  fn add_language_change_listener(self: I18nManager, listener: fn(String, String)) {
    self.listeners.push(listener)
  }
  
  fn get_current_locale(self: I18nManager) -> String {
    self.current_locale
  }
  
  fn persist_locale_preference(self: I18nManager) {
    // Mock persistence
  }
  
  fn load_locale_preference(self: I18nManager) {
    // Mock loading
  }
  
  fn detect_locale_from_browser(self: I18nManager, browser_locales: Array(String)) -> String {
    // Mock browser locale detection
    for locale in browser_locales {
      if locale == "zh-CN" or locale == "en-US" or locale == "es-ES" {
        return locale
      }
    }
    "en-US"  // Fallback
  }
  
  fn detect_locale_from_system(self: I18nManager) -> String {
    // Mock system locale detection
    "en-US"
  }
  
  fn auto_detect_locale(self: I18nManager) {
    // Mock auto-detection
    self.set_locale("en-US")
  }
  
  fn register_bundle(self: I18nManager, bundle: ResourceBundle) -> Result(Bool, I18nError) {
    self.bundles.insert(bundle.name, bundle)
    Ok(true)
  }
  
  fn translate_from_bundle(self: I18nManager, bundle_name: String, key: String) -> String {
    // Mock bundle translation
    match self.bundles.get(bundle_name) {
      Some(bundle) => {
        match bundle.locales.find(fn(l) { l.0 == self.current_locale }) {
          Some((_, strings)) => {
            for (k, v) in strings {
              if k == key {
                return v
              }
            }
            key  // Fallback to key
          }
          None => key  // Fallback to key
        }
      }
      None => key  // Fallback to key
    }
  }
  
  fn unload_bundle(self: I18nManager, bundle_name: String) -> Result(Bool, I18nError) {
    self.bundles.remove(bundle_name)
    Ok(true)
  }
  
  fn reload_bundle(self: I18nManager, bundle_name: String) -> Result(Bool, I18nError) {
    // Mock bundle reload
    Ok(true)
  }
  
  fn update_bundle(self: I18nManager, bundle: ResourceBundle) -> Result(Bool, I18nError) {
    self.bundles.insert(bundle.name, bundle)
    Ok(true)
  }
  
  fn get_bundle_statistics(self: I18nManager) -> BundleStatistics {
    BundleStatistics {
      total_bundles: self.bundles.length(),
      loaded_bundles: self.bundles.length(),
      total_translations: 100,  // Mock
      total_locales: 3  // Mock
    }
  }
}

type I18nError {
  DependencyError
}

type DateTime {
  year: Int
  month: Int
  day: Int
  hour: Int
  minute: Int
  second: Int
  timezone: String
}

impl Array(T) {
  fn find(self: Array(T), predicate: fn(T) -> Bool) -> Option(T) {
    for item in self {
      if predicate(item) {
        return Some(item)
      }
    }
    None
  }
  
  fn length(self: Array(T)) -> Int {
    self.length()
  }
}

impl String {
  fn replace(self: String, from: String, to: String) -> String {
    // Mock string replace
    if self.contains(from) {
      // Simplified implementation
      self
    } else {
      self
    }
  }
  
  fn contains(self: String, substring: String) -> Bool {
    // Mock contains
    false
  }
}