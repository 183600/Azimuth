// Azimuth 国际化和本地化测试用例
// 专注于多语言支持和地区适配验证

// 测试1: 多语言支持测试
test "多语言支持测试" {
  // 创建国际化资源管理器
  let i18n_manager = I18nManager::new()
  
  // 加载不同语言的资源文件
  I18nManager::load_resource(i18n_manager, "en", "resources/messages_en.properties")
  I18nManager::load_resource(i18n_manager, "zh", "resources/messages_zh.properties")
  I18nManager::load_resource(i18n_manager, "ja", "resources/messages_ja.properties")
  I18nManager::load_resource(i18n_manager, "fr", "resources/messages_fr.properties")
  I18nManager::load_resource(i18n_manager, "de", "resources/messages_de.properties")
  I18nManager::load_resource(i18n_manager, "es", "resources/messages_es.properties")
  I18nManager::load_resource(i18n_manager, "ru", "resources/messages_ru.properties")
  I18nManager::load_resource(i18n_manager, "ar", "resources/messages_ar.properties")
  
  // 测试英文本地化
  let en_messages = I18nManager::get_messages(i18n_manager, "en")
  assert_true(en_messages.length() > 0)
  
  let en_welcome = I18nManager::get_message(i18n_manager, "en", "welcome.message")
  assert_eq(en_welcome, Some("Welcome to Azimuth Telemetry System"))
  
  let en_error = I18nManager::get_message(i18n_manager, "en", "error.connection.failed")
  assert_eq(en_error, Some("Connection failed: {0}"))
  
  // 测试中文本地化
  let zh_messages = I18nManager::get_messages(i18n_manager, "zh")
  assert_true(zh_messages.length() > 0)
  
  let zh_welcome = I18nManager::get_message(i18n_manager, "zh", "welcome.message")
  assert_eq(zh_welcome, Some("欢迎使用Azimuth遥测系统"))
  
  let zh_error = I18nManager::get_message(i18n_manager, "zh", "error.connection.failed")
  assert_eq(zh_error, Some("连接失败：{0}"))
  
  // 测试日文本地化
  let ja_messages = I18nManager::get_messages(i18n_manager, "ja")
  assert_true(ja_messages.length() > 0)
  
  let ja_welcome = I18nManager::get_message(i18n_manager, "ja", "welcome.message")
  assert_eq(ja_welcome, Some("Azimuthテレメトリシステムへようこそ"))
  
  let ja_error = I18nManager::get_message(i18n_manager, "ja", "error.connection.failed")
  assert_eq(ja_error, Some("接続に失敗しました：{0}"))
  
  // 测试法文本地化
  let fr_messages = I18nManager::get_messages(i18n_manager, "fr")
  assert_true(fr_messages.length() > 0)
  
  let fr_welcome = I18nManager::get_message(i18n_manager, "fr", "welcome.message")
  assert_eq(fr_welcome, Some("Bienvenue dans le système de télémétrie Azimuth"))
  
  let fr_error = I18nManager::get_message(i18n_manager, "fr", "error.connection.failed")
  assert_eq(fr_error, Some("Échec de la connexion : {0}"))
  
  // 测试德文本地化
  let de_messages = I18nManager::get_messages(i18n_manager, "de")
  assert_true(de_messages.length() > 0)
  
  let de_welcome = I18nManager::get_message(i18n_manager, "de", "welcome.message")
  assert_eq(de_welcome, Some("Willkommen beim Azimuth-Telemetriesystem"))
  
  let de_error = I18nManager::get_message(i18n_manager, "de", "error.connection.failed")
  assert_eq(de_error, Some("Verbindung fehlgeschlagen: {0}"))
  
  // 测试西班牙文本地化
  let es_messages = I18nManager::get_messages(i18n_manager, "es")
  assert_true(es_messages.length() > 0)
  
  let es_welcome = I18nManager::get_message(i18n_manager, "es", "welcome.message")
  assert_eq(es_welcome, Some("Bienvenido al sistema de telemetría Azimuth"))
  
  let es_error = I18nManager::get_message(i18n_manager, "es", "error.connection.failed")
  assert_eq(es_error, Some("Error de conexión: {0}"))
  
  // 测试俄文本地化
  let ru_messages = I18nManager::get_messages(i18n_manager, "ru")
  assert_true(ru_messages.length() > 0)
  
  let ru_welcome = I18nManager::get_message(i18n_manager, "ru", "welcome.message")
  assert_eq(ru_welcome, Some("Добро пожаловать в систему телеметрии Azimuth"))
  
  let ru_error = I18nManager::get_message(i18n_manager, "ru", "error.connection.failed")
  assert_eq(ru_error, Some("Ошибка подключения: {0}"))
  
  // 测试阿拉伯文本地化（从右到左）
  let ar_messages = I18nManager::get_messages(i18n_manager, "ar")
  assert_true(ar_messages.length() > 0)
  
  let ar_welcome = I18nManager::get_message(i18n_manager, "ar", "welcome.message")
  assert_eq(ar_welcome, Some("مرحبا بك في نظام قياس الأبعاد Azimuth"))
  
  let ar_error = I18nManager::get_message(i18n_manager, "ar", "error.connection.failed")
  assert_eq(ar_error, Some("فشل الاتصال: {0}"))
  
  // 验证阿拉伯语文本方向
  assert_true(I18nManager::is_rtl(i18n_manager, "ar"))
  assert_false(I18nManager::is_rtl(i18n_manager, "en"))
  assert_false(I18nManager::is_rtl(i18n_manager, "zh"))
}

// 测试2: 消息格式化测试
test "消息格式化测试" {
  let i18n_manager = I18nManager::new()
  
  // 加载资源文件
  I18nManager::load_resource(i18n_manager, "en", "resources/messages_en.properties")
  I18nManager::load_resource(i18n_manager, "zh", "resources/messages_zh.properties")
  I18nManager::load_resource(i18n_manager, "ja", "resources/messages_ja.properties")
  
  // 测试英文消息格式化
  let en_template = I18nManager::get_message(i18n_manager, "en", "user.greeting")
  assert_eq(en_template, Some("Hello, {0}! You have {1} new messages."))
  
  let en_formatted = I18nManager::format_message(i18n_manager, "en", "user.greeting", ["Alice", "5"])
  assert_eq(en_formatted, Some("Hello, Alice! You have 5 new messages."))
  
  // 测试中文消息格式化
  let zh_template = I18nManager::get_message(i18n_manager, "zh", "user.greeting")
  assert_eq(zh_template, Some("你好，{0}！您有{1}条新消息。"))
  
  let zh_formatted = I18nManager::format_message(i18n_manager, "zh", "user.greeting", ["爱丽丝", "5"])
  assert_eq(zh_formatted, Some("你好，爱丽丝！您有5条新消息。"))
  
  // 测试日文消息格式化
  let ja_template = I18nManager::get_message(i18n_manager, "ja", "user.greeting")
  assert_eq(ja_template, Some("{0}さん、こんにちは！{1}件の新しいメッセージがあります。"))
  
  let ja_formatted = I18nManager::format_message(i18n_manager, "ja", "user.greeting", ["アリス", "5"])
  assert_eq(ja_formatted, Some("アリスさん、こんにちは！5件の新しいメッセージがあります。"))
  
  // 测试复数形式处理
  let en_plural_template = I18nManager::get_message(i18n_manager, "en", "item.count")
  assert_eq(en_plural_template, Some("You have {0, plural, one{# item} other{# items}}."))
  
  let en_singular = I18nManager::format_message(i18n_manager, "en", "item.count", ["1"])
  assert_eq(en_singular, Some("You have 1 item."))
  
  let en_plural = I18nManager::format_message(i18n_manager, "en", "item.count", ["5"])
  assert_eq(en_plural, Some("You have 5 items."))
  
  // 测试日期格式化
  let test_date = DateTime::new(2023, 1, 15, 14, 30, 0)
  
  let en_date = I18nManager::format_date(i18n_manager, "en", test_date, "medium")
  assert_eq(en_date, Some("Jan 15, 2023, 2:30:00 PM"))
  
  let zh_date = I18nManager::format_date(i18n_manager, "zh", test_date, "medium")
  assert_eq(zh_date, Some("2023年1月15日 下午2:30:00"))
  
  let ja_date = I18nManager::format_date(i18n_manager, "ja", test_date, "medium")
  assert_eq(ja_date, Some("2023/01/15 14:30:00"))
  
  // 测试数字格式化
  let en_number = I18nManager::format_number(i18n_manager, "en", 1234567.89, "currency", "USD")
  assert_eq(en_number, Some("$1,234,567.89"))
  
  let zh_number = I18nManager::format_number(i18n_manager, "zh", 1234567.89, "currency", "CNY")
  assert_eq(zh_number, Some("¥1,234,567.89"))
  
  let ja_number = I18nManager::format_number(i18n_manager, "ja", 1234567.89, "currency", "JPY")
  assert_eq(ja_number, Some("¥1,234,568"))  // 日元没有小数
  
  // 测试百分比格式化
  let en_percent = I18nManager::format_number(i18n_manager, "en", 0.75, "percent", "")
  assert_eq(en_percent, Some("75%"))
  
  let zh_percent = I18nManager::format_number(i18n_manager, "zh", 0.75, "percent", "")
  assert_eq(zh_percent, Some("75%"))
  
  let ja_percent = I18nManager::format_number(i18n_manager, "ja", 0.75, "percent", "")
  assert_eq(ja_percent, Some("75%"))
}

// 测试3: 地区设置测试
test "地区设置测试" {
  let locale_manager = LocaleManager::new()
  
  // 测试不同地区设置
  let en_us = Locale::new("en", "US")
  let en_gb = Locale::new("en", "GB")
  let zh_cn = Locale::new("zh", "CN")
  let zh_tw = Locale::new("zh", "TW")
  let ja_jp = Locale::new("ja", "JP")
  let fr_fr = Locale::new("fr", "FR")
  let de_de = Locale::new("de", "DE")
  let es_es = Locale::new("es", "ES")
  let ru_ru = Locale::new("ru", "RU")
  let ar_sa = Locale::new("ar", "SA")
  
  // 测试地区代码
  assert_eq(Locale::language(en_us), "en")
  assert_eq(Locale::country(en_us), "US")
  assert_eq(Locale::to_string(en_us), "en_US")
  
  assert_eq(Locale::language(zh_cn), "zh")
  assert_eq(Locale::country(zh_cn), "CN")
  assert_eq(Locale::to_string(zh_cn), "zh_CN")
  
  // 测试地区特定格式
  let test_date = DateTime::new(2023, 1, 15, 14, 30, 0)
  
  let en_us_date = LocaleManager::format_date(locale_manager, en_us, test_date, "short")
  assert_eq(en_us_date, Some("1/15/23, 2:30 PM"))
  
  let en_gb_date = LocaleManager::format_date(locale_manager, en_gb, test_date, "short")
  assert_eq(en_gb_date, Some("15/01/2023, 14:30"))
  
  let zh_cn_date = LocaleManager::format_date(locale_manager, zh_cn, test_date, "short")
  assert_eq(zh_cn_date, Some("2023/1/15 下午2:30"))
  
  let zh_tw_date = LocaleManager::format_date(locale_manager, zh_tw, test_date, "short")
  assert_eq(zh_tw_date, Some("2023/1/15 下午2:30"))
  
  // 测试地区特定数字格式
  let en_us_number = LocaleManager::format_number(locale_manager, en_us, 1234567.89, "decimal", "")
  assert_eq(en_us_number, Some("1,234,567.89"))
  
  let en_gb_number = LocaleManager::format_number(locale_manager, en_gb, 1234567.89, "decimal", "")
  assert_eq(en_gb_number, Some("1,234,567.89"))
  
  let de_de_number = LocaleManager::format_number(locale_manager, de_de, 1234567.89, "decimal", "")
  assert_eq(de_de_number, Some("1.234.567,89"))  // 德语使用点作为千位分隔符，逗号作为小数点
  
  let fr_fr_number = LocaleManager::format_number(locale_manager, fr_fr, 1234567.89, "decimal", "")
  assert_eq(fr_fr_number, Some("1 234 567,89"))  // 法语使用空格作为千位分隔符
  
  // 测试地区特定货币格式
  let en_us_currency = LocaleManager::format_currency(locale_manager, en_us, 1234.56, "USD")
  assert_eq(en_us_currency, Some("$1,234.56"))
  
  let en_gb_currency = LocaleManager::format_currency(locale_manager, en_gb, 1234.56, "GBP")
  assert_eq(en_gb_currency, Some("£1,234.56"))
  
  let zh_cn_currency = LocaleManager::format_currency(locale_manager, zh_cn, 1234.56, "CNY")
  assert_eq(zh_cn_currency, Some("¥1,234.56"))
  
  let ja_jp_currency = LocaleManager::format_currency(locale_manager, ja_jp, 1234.56, "JPY")
  assert_eq(ja_jp_currency, Some("¥1,235"))  // 日元没有小数
  
  // 测试地区特定时间格式
  let test_time = Time::new(14, 30, 0)
  
  let en_us_time = LocaleManager::format_time(locale_manager, en_us, test_time, "short")
  assert_eq(en_us_time, Some("2:30 PM"))
  
  let en_gb_time = LocaleManager::format_time(locale_manager, en_gb, test_time, "short")
  assert_eq(en_gb_time, Some("14:30"))
  
  let de_de_time = LocaleManager::format_time(locale_manager, de_de, test_time, "short")
  assert_eq(de_de_time, Some("14:30"))
  
  // 测试地区特定排序规则
  let strings = ["apple", "banana", "cherry", "date"]
  
  let en_sort = LocaleManager::sort_strings(locale_manager, en_us, strings)
  assert_eq(en_sort, ["apple", "banana", "cherry", "date"])
  
  let zh_sort = LocaleManager::sort_strings(locale_manager, zh_cn, strings)
  // 中文排序可能基于拼音或其他规则
  assert_eq(zh_sort, ["apple", "banana", "cherry", "date"])
  
  let ar_sort = LocaleManager::sort_strings(locale_manager, ar_sa, strings)
  // 阿拉伯语排序可能不同
  assert_eq(ar_sort, ["apple", "banana", "cherry", "date"])
}

// 测试4: 文本方向测试
test "文本方向测试" {
  let i18n_manager = I18nManager::new()
  
  // 加载资源文件
  I18nManager::load_resource(i18n_manager, "en", "resources/messages_en.properties")
  I18nManager::load_resource(i18n_manager, "zh", "resources/messages_zh.properties")
  I18nManager::load_resource(i18n_manager, "ja", "resources/messages_ja.properties")
  I18nManager::load_resource(i18n_manager, "ar", "resources/messages_ar.properties")
  I18nManager::load_resource(i18n_manager, "he", "resources/messages_he.properties")
  
  // 测试从左到右语言
  assert_false(I18nManager::is_rtl(i18n_manager, "en"))
  assert_false(I18nManager::is_rtl(i18n_manager, "zh"))
  assert_false(I18nManager::is_rtl(i18n_manager, "ja"))
  assert_false(I18nManager::is_rtl(i18n_manager, "fr"))
  assert_false(I18nManager::is_rtl(i18n_manager, "de"))
  assert_false(I18nManager::is_rtl(i18n_manager, "es"))
  assert_false(I18nManager::is_rtl(i18n_manager, "ru"))
  
  // 测试从右到左语言
  assert_true(I18nManager::is_rtl(i18n_manager, "ar"))
  assert_true(I18nManager::is_rtl(i18n_manager, "he"))
  assert_true(I18nManager::is_rtl(i18n_manager, "fa"))  // 波斯语
  assert_true(I18nManager::is_rtl(i18n_manager, "ur"))  // 乌尔都语
  
  // 测试文本方向检测
  let ltr_text = "Hello, world!"
  let rtl_text = "مرحبا بالعالم!"
  let mixed_text = "Hello, مرحبا بالعالم!"
  
  assert_eq(I18nManager::detect_text_direction(i18n_manager, ltr_text), "ltr")
  assert_eq(I18nManager::detect_text_direction(i18n_manager, rtl_text), "rtl")
  assert_eq(I18nManager::detect_text_direction(i18n_manager, mixed_text), "mixed")
  
  // 测试文本对齐
  let ltr_alignment = I18nManager::get_text_alignment(i18n_manager, "en", "start")
  assert_eq(ltr_alignment, "left")
  
  let rtl_alignment = I18nManager::get_text_alignment(i18n_manager, "ar", "start")
  assert_eq(rtl_alignment, "right")
  
  let ltr_end_alignment = I18nManager::get_text_alignment(i18n_manager, "en", "end")
  assert_eq(ltr_end_alignment, "right")
  
  let rtl_end_alignment = I18nManager::get_text_alignment(i18n_manager, "ar", "end")
  assert_eq(rtl_end_alignment, "left")
  
  // 测试布局方向
  let ltr_layout = I18nManager::get_layout_direction(i18n_manager, "en")
  assert_eq(ltr_layout, "ltr")
  
  let rtl_layout = I18nManager::get_layout_direction(i18n_manager, "ar")
  assert_eq(rtl_layout, "rtl")
  
  // 测试边距和填充调整
  let ltr_margin = I18nManager::adjust_margin_for_direction(i18n_manager, "en", {
    top: 10,
    right: 20,
    bottom: 10,
    left: 20
  })
  assert_eq(ltr_margin, { top: 10, right: 20, bottom: 10, left: 20 })
  
  let rtl_margin = I18nManager::adjust_margin_for_direction(i18n_manager, "ar", {
    top: 10,
    right: 20,
    bottom: 10,
    left: 20
  })
  assert_eq(rtl_margin, { top: 10, right: 20, bottom: 10, left: 20 })
  
  // 测试图标翻转
  let ltr_icons = I18nManager::adjust_icons_for_direction(i18n_manager, "en", ["arrow-left", "arrow-right"])
  assert_eq(ltr_icons, ["arrow-left", "arrow-right"])
  
  let rtl_icons = I18nManager::adjust_icons_for_direction(i18n_manager, "ar", ["arrow-left", "arrow-right"])
  assert_eq(rtl_icons, ["arrow-right", "arrow-left"])  // 方向图标应该翻转
}

// 测试5: 时区处理测试
test "时区处理测试" {
  let timezone_manager = TimezoneManager::new()
  
  // 测试不同时区
  let utc = Timezone::new("UTC")
  let us_eastern = Timezone::new("America/New_York")
  let us_western = Timezone::new("America/Los_Angeles")
  let europe_london = Timezone::new("Europe/London")
  let europe_paris = Timezone::new("Europe/Paris")
  let asia_shanghai = Timezone::new("Asia/Shanghai")
  let asia_tokyo = Timezone::new("Asia/Tokyo")
  let australia_sydney = Timezone::new("Australia/Sydney")
  
  // 创建测试时间（UTC时间2023-01-15 14:30:00）
  let utc_time = DateTime::new(2023, 1, 15, 14, 30, 0)
  
  // 测试时区转换
  let us_eastern_time = TimezoneManager::convert(timezone_manager, utc_time, utc, us_eastern)
  // 1月15日是冬天，纽约是UTC-5
  assert_eq(us_eastern_time, DateTime::new(2023, 1, 15, 9, 30, 0))
  
  let us_western_time = TimezoneManager::convert(timezone_manager, utc_time, utc, us_western)
  // 1月15日是冬天，洛杉矶是UTC-8
  assert_eq(us_western_time, DateTime::new(2023, 1, 15, 6, 30, 0))
  
  let europe_london_time = TimezoneManager::convert(timezone_manager, utc_time, utc, europe_london)
  // 1月15日是冬天，伦敦是UTC+0
  assert_eq(europe_london_time, DateTime::new(2023, 1, 15, 14, 30, 0))
  
  let europe_paris_time = TimezoneManager::convert(timezone_manager, utc_time, utc, europe_paris)
  // 1月15日是冬天，巴黎是UTC+1
  assert_eq(europe_paris_time, DateTime::new(2023, 1, 15, 15, 30, 0))
  
  let asia_shanghai_time = TimezoneManager::convert(timezone_manager, utc_time, utc, asia_shanghai)
  // 上海是UTC+8
  assert_eq(asia_shanghai_time, DateTime::new(2023, 1, 15, 22, 30, 0))
  
  let asia_tokyo_time = TimezoneManager::convert(timezone_manager, utc_time, utc, asia_tokyo)
  // 东京是UTC+9
  assert_eq(asia_tokyo_time, DateTime::new(2023, 1, 15, 23, 30, 0))
  
  let australia_sydney_time = TimezoneManager::convert(timezone_manager, utc_time, utc, australia_sydney)
  // 1月15日是夏天，悉尼是UTC+11
  assert_eq(australia_sydney_time, DateTime::new(2023, 1, 16, 1, 30, 0))
  
  // 测试夏令时处理
  let summer_utc_time = DateTime::new(2023, 7, 15, 14, 30, 0)
  
  let us_eastern_summer = TimezoneManager::convert(timezone_manager, summer_utc_time, utc, us_eastern)
  // 7月15日是夏天，纽约是UTC-4（夏令时）
  assert_eq(us_eastern_summer, DateTime::new(2023, 7, 15, 10, 30, 0))
  
  let europe_london_summer = TimezoneManager::convert(timezone_manager, summer_utc_time, utc, europe_london)
  // 7月15日是夏天，伦敦是UTC+1（夏令时）
  assert_eq(europe_london_summer, DateTime::new(2023, 7, 15, 15, 30, 0))
  
  let europe_paris_summer = TimezoneManager::convert(timezone_manager, summer_utc_time, utc, europe_paris)
  // 7月15日是夏天，巴黎是UTC+2（夏令时）
  assert_eq(europe_paris_summer, DateTime::new(2023, 7, 15, 16, 30, 0))
  
  // 测试时区格式化
  let en_us_locale = Locale::new("en", "US")
  
  let utc_formatted = TimezoneManager::format_with_timezone(timezone_manager, utc_time, utc, en_us_locale, "full")
  assert_eq(utc_formatted, Some("Sunday, January 15, 2023 at 2:30:00 PM UTC"))
  
  let us_eastern_formatted = TimezoneManager::format_with_timezone(timezone_manager, utc_time, us_eastern, en_us_locale, "full")
  assert_eq(us_eastern_formatted, Some("Sunday, January 15, 2023 at 9:30:00 AM EST"))
  
  let asia_shanghai_formatted = TimezoneManager::format_with_timezone(timezone_manager, utc_time, asia_shanghai, en_us_locale, "full")
  assert_eq(asia_shanghai_formatted, Some("Sunday, January 15, 2023 at 10:30:00 PM CST"))
  
  // 测试时区偏移计算
  let us_eastern_offset = TimezoneManager::get_offset(timezone_manager, us_eastern, utc_time)
  assert_eq(us_eastern_offset, -18000)  // -5小时 = -18000秒
  
  let asia_shanghai_offset = TimezoneManager::get_offset(timezone_manager, asia_shanghai, utc_time)
  assert_eq(asia_shanghai_offset, 28800)  // +8小时 = 28800秒
  
  // 测试时区名称
  let us_eastern_name = TimezoneManager::get_name(timezone_manager, us_eastern, utc_time)
  assert_eq(us_eastern_name, Some("Eastern Standard Time"))
  
  let us_eastern_summer_name = TimezoneManager::get_name(timezone_manager, us_eastern, summer_utc_time)
  assert_eq(us_eastern_summer_name, Some("Eastern Daylight Time"))
  
  let asia_shanghai_name = TimezoneManager::get_name(timezone_manager, asia_shanghai, utc_time)
  assert_eq(asia_shanghai_name, Some("China Standard Time"))
}

// 测试6: 字符集和编码测试
test "字符集和编码测试" {
  let encoding_manager = EncodingManager::new()
  
  // 测试不同字符集
  let utf8_text = "Hello, 世界! こんにちは! مرحبا! Bonjour! ¡Hola!"
  let utf16_text = EncodingManager::convert_encoding(encoding_manager, utf8_text, "UTF-8", "UTF-16")
  let iso88591_text = EncodingManager::convert_encoding(encoding_manager, "Hello, world!", "UTF-8", "ISO-8859-1")
  
  // 测试编码检测
  let detected_utf8 = EncodingManager::detect_encoding(encoding_manager, utf8_text)
  assert_eq(detected_utf8, Some("UTF-8"))
  
  let detected_utf16 = EncodingManager::detect_encoding(encoding_manager, utf16_text)
  assert_eq(detected_utf16, Some("UTF-16"))
  
  // 测试编码转换
  let back_to_utf8 = EncodingManager::convert_encoding(encoding_manager, utf16_text, "UTF-16", "UTF-8")
  assert_eq(back_to_utf8, utf8_text)
  
  // 测试特殊字符处理
  let special_chars = "éàèùâêîôûëïüÿçñåøßæœ"
  let encoded_special = EncodingManager::encode_utf8(encoding_manager, special_chars)
  let decoded_special = EncodingManager::decode_utf8(encoding_manager, encoded_special)
  assert_eq(decoded_special, special_chars)
  
  // 测试表情符号
  let emoji_text = "😀😃😄😁😆😅😂🤣😊😇🙂🙃😉😌😍🥰😘😗😙😚😋😛😝😜🤪🤨🧐🤓😎🤩🥳😏😒😞😔😟😕🙁☹️😣😖😫😩🥺😢😭😤😠😡🤬🤯😳🥵🥶😱😨😰😥😓🤗🤔🤭🤫🤥😶😐😑😬🙄😯😦😧😮😲🥱😴🤤😪😵🤐🥴🤢🤮🤧😷🤒🤕🤑🤠😈👿👹👺🤡💩👻💀☠️👽👾🤖🎃😺😸😹😻😼😽🙀😿😾")
  let encoded_emoji = EncodingManager::encode_utf8(encoding_manager, emoji_text)
  let decoded_emoji = EncodingManager::decode_utf8(encoding_manager, encoded_emoji)
  assert_eq(decoded_emoji, emoji_text)
  
  // 测试组合字符
  let combining_chars = "e\u0301a\u0300o\u0302u\u0308"  // éàôü
  let normalized = EncodingManager::normalize_unicode(encoding_manager, combining_chars, "NFC")
  assert_eq(normalized, "éàôü")
  
  // 测试大小写转换（考虑不同语言）
  let english_text = "Hello World"
  let english_upper = EncodingManager::to_upper_case(encoding_manager, english_text, "en")
  let english_lower = EncodingManager::to_lower_case(encoding_manager, english_text, "en")
  assert_eq(english_upper, "HELLO WORLD")
  assert_eq(english_lower, "hello world")
  
  let turkish_text = "ıİgĞ"  // 土耳其语特殊字符
  let turkish_upper = EncodingManager::to_upper_case(encoding_manager, turkish_text, "tr")
  let turkish_lower = EncodingManager::to_lower_case(encoding_manager, turkish_text, "tr")
  assert_eq(turkish_upper, "IİGĞ")
  assert_eq(turkish_lower, "ıiğğ")
  
  // 测试文本宽度计算（考虑全角字符）
  let latin_text = "Hello"
  let latin_width = EncodingManager::get_text_width(encoding_manager, latin_text)
  assert_eq(latin_width, 5)  // 拉丁字符每个占1个单位宽度
  
  let chinese_text = "你好世界"
  let chinese_width = EncodingManager::get_text_width(encoding_manager, chinese_text)
  assert_eq(chinese_width, 8)  // 中文字符每个占2个单位宽度
  
  let mixed_text = "Hello世界"
  let mixed_width = EncodingManager::get_text_width(encoding_manager, mixed_text)
  assert_eq(mixed_width, 9)  // 5 + 4
  
  // 测试字符串截断（考虑字符边界）
  let long_text = "Hello, 世界! This is a long text that needs to be truncated."
  let truncated = EncodingManager::truncate_by_width(encoding_manager, long_text, 20, "...")
  assert_true(truncated.length() <= 23)  // 20 + "..."长度
  assert_true(truncated.ends_with("..."))
  
  // 测试双向文本处理
  let bidi_text = "Hello مرحبا World"
  let bidi_visual = EncodingManager::to_visual_order(encoding_manager, bidi_text)
  let bidi_logical = EncodingManager::to_logical_order(encoding_manager, bidi_visual)
  assert_eq(bidi_logical, bidi_text)
}

// 测试7: 遥测数据国际化测试
test "遥测数据国际化测试" {
  let i18n_manager = I18nManager::new()
  
  // 加载资源文件
  I18nManager::load_resource(i18n_manager, "en", "resources/messages_en.properties")
  I18nManager::load_resource(i18n_manager, "zh", "resources/messages_zh.properties")
  I18nManager::load_resource(i18n_manager, "ja", "resources/messages_ja.properties")
  
  // 创建国际化遥测组件
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "i18n.telemetry.test")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "i18n.telemetry.test")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "i18n.telemetry.test")
  
  // 测试多语言Span名称
  let en_span = Tracer::start_span(tracer, "user.login")
  let zh_span = Tracer::start_span(tracer, "用户登录")
  let ja_span = Tracer::start_span(tracer, "ユーザーログイン")
  
  // 设置多语言属性
  Span::set_attribute(en_span, "operation.name.en", StringValue("user.login"))
  Span::set_attribute(en_span, "operation.name.zh", StringValue("用户登录"))
  Span::set_attribute(en_span, "operation.name.ja", StringValue("ユーザーログイン"))
  
  Span::set_attribute(zh_span, "operation.name.en", StringValue("user.login"))
  Span::set_attribute(zh_span, "operation.name.zh", StringValue("用户登录"))
  Span::set_attribute(zh_span, "operation.name.ja", StringValue("ユーザーログイン"))
  
  Span::set_attribute(ja_span, "operation.name.en", StringValue("user.login"))
  Span::set_attribute(ja_span, "operation.name.zh", StringValue("用户登录"))
  Span::set_attribute(ja_span, "operation.name.ja", StringValue("ユーザーログイン"))
  
  // 测试多语言事件
  let en_event_attrs = Attributes::new()
  Attributes::set(en_event_attrs, "event.message.en", StringValue("User authentication successful"))
  Attributes::set(en_event_attrs, "event.message.zh", StringValue("用户认证成功"))
  Attributes::set(en_event_attrs, "event.message.ja", StringValue("ユーザー認証成功"))
  
  Span::add_event(en_span, "authentication.success", Some(en_event_attrs))
  
  // 测试多语言指标名称
  let en_counter = Meter::create_counter(meter, "user.login.attempts")
  let zh_counter = Meter::create_counter(meter, "用户登录尝试次数")
  let ja_counter = Meter::create_counter(meter, "ユーザーログイン試行回数")
  
  // 记录多语言指标
  let en_attrs = [("locale", StringValue("en"))]
  let zh_attrs = [("locale", StringValue("zh"))]
  let ja_attrs = [("locale", StringValue("ja"))]
  
  Counter::add(en_counter, 1.0, Some(en_attrs))
  Counter::add(zh_counter, 1.0, Some(zh_attrs))
  Counter::add(ja_counter, 1.0, Some(ja_attrs))
  
  // 测试多语言日志
  let en_log_attrs = Attributes::new()
  Attributes::set(en_log_attrs, "locale", StringValue("en"))
  Attributes::set(en_log_attrs, "log.message.en", StringValue("User login attempt from IP: {0}"))
  Attributes::set(en_log_attrs, "log.message.zh", StringValue("用户从IP {0} 尝试登录"))
  Attributes::set(en_log_attrs, "log.message.ja", StringValue("IP {0} からのユーザーログイン試行"))
  
  let en_log_record = LogRecord::new(
    Info,
    Some("User login attempt from IP: 192.168.1.1"),
    Some(en_log_attrs),
    Some(Time::now()),
    Some(Time::now() + 1),
    Some("i18n-test-trace"),
    Some("i18n-test-span")
  )
  Logger::emit(logger, en_log_record)
  
  // 验证多语言遥测数据
  let en_name = Span::get_attribute(en_span, "operation.name.en")
  match en_name {
    StringValue(value) => assert_eq(value, "user.login")
    _ => assert_true(false)
  }
  
  let zh_name = Span::get_attribute(zh_span, "operation.name.zh")
  match zh_name {
    StringValue(value) => assert_eq(value, "用户登录")
    _ => assert_true(false)
  }
  
  let ja_name = Span::get_attribute(ja_span, "operation.name.ja")
  match ja_name {
    StringValue(value) => assert_eq(value, "ユーザーログイン")
    _ => assert_true(false)
  }
  
  // 结束Span
  Span::end(en_span)
  Span::end(zh_span)
  Span::end(ja_span)
  
  // 测试多语言仪表板
  let dashboard = I18nDashboard::new()
  
  // 设置仪表板语言
  I18nDashboard::set_language(dashboard, "en")
  let en_title = I18nDashboard::get_title(dashboard)
  assert_eq(en_title, Some("Azimuth Telemetry Dashboard"))
  
  I18nDashboard::set_language(dashboard, "zh")
  let zh_title = I18nDashboard::get_title(dashboard)
  assert_eq(zh_title, Some("Azimuth 遥测仪表板"))
  
  I18nDashboard::set_language(dashboard, "ja")
  let ja_title = I18nDashboard::get_title(dashboard)
  assert_eq(ja_title, Some("Azimuth テレメトリーダッシュボード"))
}