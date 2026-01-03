// Azimuth 国际化支持测试
// 专注于测试多语言、本地化和全球化功能

// 语言和地区类型
type Language {
  English
  Chinese
  Spanish
  French
  German
  Japanese
  Korean
  Russian
  Arabic
  Hindi
  Unknown
}

// 地区类型
type Region {
  UnitedStates
  UnitedKingdom
  China
  Japan
  Germany
  France
  Spain
  Russia
  India
  Brazil
  UnknownRegion
}

// 本地化配置类型
type LocaleConfig = {
  language : Language,
  region : Region,
  currency : String,
  date_format : String,
  time_format : String,
  number_format : String,
  text_direction : TextDirection
}

// 文本方向类型
type TextDirection {
  LeftToRight
  RightToLeft
  TopToBottom
}

// 测试1: 多语言文本支持
test "多语言文本支持测试" {
  // 测试不同语言的文本显示
  let english_text = get_localized_text("welcome_message", English)
  let chinese_text = get_localized_text("welcome_message", Chinese)
  let spanish_text = get_localized_text("welcome_message", Spanish)
  let arabic_text = get_localized_text("welcome_message", Arabic)
  
  // 验证不同语言的文本
  assert_eq(english_text, "Welcome to our application!")
  assert_eq(chinese_text, "欢迎使用我们的应用程序！")
  assert_eq(spanish_text, "¡Bienvenido a nuestra aplicación!")
  assert_eq(arabic_text, "مرحبا بكم في تطبيقنا!")
  
  // 测试参数化文本
  let param_english = get_parameterized_text("user_greeting", English, [("name", "John")])
  let param_chinese = get_parameterized_text("user_greeting", Chinese, [("name", "张三")])
  
  assert_eq(param_english, "Hello, John!")
  assert_eq(param_chinese, "你好，张三！")
  
  // 测试复数形式
  let singular_english = get_pluralized_text("item_count", English, 1)
  let plural_english = get_pluralized_text("item_count", English, 5)
  let singular_chinese = get_pluralized_text("item_count", Chinese, 1)
  let plural_chinese = get_pluralized_text("item_count", Chinese, 5)
  
  assert_eq(singular_english, "You have 1 item")
  assert_eq(plural_english, "You have 5 items")
  assert_eq(singular_chinese, "您有1个项目")
  assert_eq(plural_chinese, "您有5个项目")
  
  // 测试性别化文本
  let male_text = get_gendered_text("user_title", English, "male")
  let female_text = get_gendered_text("user_title", English, "female")
  let neutral_text = get_gendered_text("user_title", English, "neutral")
  
  assert_eq(male_text, "Mr.")
  assert_eq(female_text, "Ms.")
  assert_eq(neutral_text, "Mx.")
}

// 测试2: 日期和时间本地化
test "日期和时间本地化测试" {
  // 创建测试日期
  let test_date = create_date_time(2023, 1, 15, 14, 30, 45)
  
  // 测试不同地区的日期格式
  let us_date = format_date(test_date, UnitedStates)
  let uk_date = format_date(test_date, UnitedKingdom)
  let chinese_date = format_date(test_date, China)
  let japanese_date = format_date(test_date, Japan)
  
  assert_eq(us_date, "1/15/2023")
  assert_eq(uk_date, "15/01/2023")
  assert_eq(chinese_date, "2023年1月15日")
  assert_eq(japanese_date, "2023/01/15")
  
  // 测试不同地区的时间格式
  let us_time = format_time(test_date, UnitedStates)
  let uk_time = format_time(test_date, UnitedKingdom)
  let chinese_time = format_time(test_date, China)
  let japanese_time = format_time(test_date, Japan)
  
  assert_eq(us_time, "2:30:45 PM")
  assert_eq(uk_time, "14:30:45")
  assert_eq(chinese_time, "下午2:30:45")
  assert_eq(japanese_time, "14:30:45")
  
  // 测试日期时间组合格式
  let us_datetime = format_date_time(test_date, UnitedStates)
  let chinese_datetime = format_date_time(test_date, China)
  
  assert_eq(us_datetime, "1/15/2023, 2:30:45 PM")
  assert_eq(chinese_datetime, "2023年1月15日 下午2:30:45")
  
  // 测试相对时间
  let past_date = create_date_time(2023, 1, 10, 14, 30, 45)
  let future_date = create_date_time(2023, 1, 20, 14, 30, 45)
  
  let past_relative = format_relative_time(past_date, test_date, English)
  let future_relative = format_relative_time(future_date, test_date, English)
  
  assert_eq(past_relative, "5 days ago")
  assert_eq(future_relative, "in 5 days")
  
  // 测试中文相对时间
  let past_relative_chinese = format_relative_time(past_date, test_date, Chinese)
  let future_relative_chinese = format_relative_time(future_date, test_date, Chinese)
  
  assert_eq(past_relative_chinese, "5天前")
  assert_eq(future_relative_chinese, "5天后")
}

// 测试3: 数字和货币本地化
test "数字和货币本地化测试" {
  // 测试数字格式
  let test_number = 1234567.89
  
  let us_number = format_number(test_number, UnitedStates)
  let german_number = format_number(test_number, Germany)
  let french_number = format_number(test_number, France)
  let chinese_number = format_number(test_number, China)
  
  assert_eq(us_number, "1,234,567.89")
  assert_eq(german_number, "1.234.567,89")
  assert_eq(french_number, "1 234 567,89")
  assert_eq(chinese_number, "1,234,567.89")
  
  // 测试百分比格式
  let percentage = 0.7542
  
  let us_percentage = format_percentage(percentage, UnitedStates)
  let chinese_percentage = format_percentage(percentage, China)
  
  assert_eq(us_percentage, "75.42%")
  assert_eq(chinese_percentage, "75.42%")
  
  // 测试货币格式
  let amount = 1234.56
  
  let usd = format_currency(amount, "USD", UnitedStates)
  let eur = format_currency(amount, "EUR", Germany)
  let gbp = format_currency(amount, "GBP", UnitedKingdom)
  let cny = format_currency(amount, "CNY", China)
  let jpy = format_currency(amount, "JPY", Japan)
  
  assert_eq(usd, "$1,234.56")
  assert_eq(eur, "1.234,56 €")
  assert_eq(gbp, "£1,234.56")
  assert_eq(cny, "¥1,234.56")
  assert_eq(jpy, "¥1,235") // 日元通常不显示小数
  
  // 测试大数字格式
  let large_number = 1234567890
  
  let us_large = format_large_number(large_number, UnitedStates)
  let chinese_large = format_large_number(large_number, China)
  
  assert_eq(us_large, "1.23B")
  assert_eq(chinese_large, "12.3亿")
}

// 测试4: 文本方向和布局
test "文本方向和布局测试" {
  // 测试不同语言的文本方向
  let english_direction = get_text_direction(English)
  let arabic_direction = get_text_direction(Arabic)
  let hebrew_direction = get_text_direction(Language.Unknown) // 假设希伯来语
  let chinese_direction = get_text_direction(Chinese)
  let japanese_direction = get_text_direction(Japanese)
  
  assert_eq(english_direction, LeftToRight)
  assert_eq(arabic_direction, RightToLeft)
  assert_eq(hebrew_direction, RightToLeft)
  assert_eq(chinese_direction, LeftToRight)
  assert_eq(japanese_direction, TopToBottom) // 传统日文可以从上到下
  
  // 测试混合方向文本处理
  let mixed_text = "Hello مرحبا World"
  let processed_text = process_mixed_direction_text(mixed_text)
  
  assert_true(processed_text.contains("Hello"))
  assert_true(processed_text.contains("مرحبا"))
  assert_true(processed_text.contains("World"))
  
  // 测试布局对齐
  let left_to_right_layout = get_layout_alignment(English)
  let right_to_left_layout = get_layout_alignment(Arabic)
  
  assert_eq(left_to_right_layout, "left")
  assert_eq(right_to_left_layout, "right")
  
  // 测试UI元素镜像
  let original_ui = {
    back_button: "← Back",
    next_button: "Next →",
    navigation: "Home | About | Contact"
  }
  
  let mirrored_ui = mirror_ui_elements(original_ui, RightToLeft)
  
  assert_eq(mirrored_ui.back_button, "Back →")
  assert_eq(mirrored_ui.next_button, "← Next")
  assert_eq(mirrored_ui.navigation, "Contact | About | Home")
}

// 测试5: 字符集和编码支持
test "字符集和编码支持测试" {
  // 测试不同字符集的文本
  let latin_text = "Hello, World!"
  let chinese_text = "你好，世界！"
  let arabic_text = "مرحبا بالعالم!"
  let russian_text = "Привет, мир!"
  let japanese_text = "こんにちは、世界！"
  let emoji_text = "🌍🚀🎉"
  
  // 验证所有文本都能正确处理
  assert_true(is_valid_unicode(latin_text))
  assert_true(is_valid_unicode(chinese_text))
  assert_true(is_valid_unicode(arabic_text))
  assert_true(is_valid_unicode(russian_text))
  assert_true(is_valid_unicode(japanese_text))
  assert_true(is_valid_unicode(emoji_text))
  
  // 测试字符计数（考虑Unicode组合字符）
  assert_eq(count_unicode_characters(latin_text), 13)
  assert_eq(count_unicode_characters(chinese_text), 6)
  assert_eq(count_unicode_characters(arabic_text), 13)
  assert_eq(count_unicode_characters(russian_text), 13)
  assert_eq(count_unicode_characters(japanese_text), 9)
  assert_eq(count_unicode_characters(emoji_text), 3)
  
  // 测试字符串长度（字节数 vs 字符数）
  assert_true(latin_text.to_utf8_bytes().length() > count_unicode_characters(latin_text))
  assert_true(chinese_text.to_utf8_bytes().length() > count_unicode_characters(chinese_text))
  assert_true(arabic_text.to_utf8_bytes().length() > count_unicode_characters(arabic_text))
  
  // 测试大小写转换（考虑不同语言的大小写规则）
  let latin_upper = latin_text.to_uppercase()
  let latin_lower = latin_text.to_lowercase()
  
  assert_true(latin_upper.contains("HELLO"))
  assert_true(latin_lower.contains("hello"))
  
  // 测试文本搜索（考虑不同语言的匹配规则）
  let latin_search = search_text(latin_text, "World")
  let chinese_search = search_text(chinese_text, "世界")
  let arabic_search = search_text(arabic_text, "العالم")
  
  assert_true(latin_search)
  assert_true(chinese_search)
  assert_true(arabic_search)
}

// 测试6: 本地化资源管理
test "本地化资源管理测试" {
  // 创建本地化管理器
  let manager = create_localization_manager()
  
  // 加载不同语言资源
  load_language_resources(manager, English)
  load_language_resources(manager, Chinese)
  load_language_resources(manager, Arabic)
  
  // 测试资源切换
  set_active_language(manager, English)
  let english_ui_text = get_ui_text(manager, "submit_button")
  assert_eq(english_ui_text, "Submit")
  
  set_active_language(manager, Chinese)
  let chinese_ui_text = get_ui_text(manager, "submit_button")
  assert_eq(chinese_ui_text, "提交")
  
  set_active_language(manager, Arabic)
  let arabic_ui_text = get_ui_text(manager, "submit_button")
  assert_eq(arabic_ui_text, "إرسال")
  
  // 测试回退机制
  set_active_language(manager, Language.Unknown)
  let fallback_text = get_ui_text(manager, "submit_button")
  assert_eq(fallback_text, "Submit") // 回退到英语
  
  // 测试资源更新
  update_language_resource(manager, English, "submit_button", "Send")
  set_active_language(manager, English)
  let updated_text = get_ui_text(manager, "submit_button")
  assert_eq(updated_text, "Send")
  
  // 测试资源缓存
  let cached_resources = get_cached_resources(manager)
  assert_true(cached_resources.contains("English"))
  assert_true(cached_resources.contains("Chinese"))
  assert_true(cached_resources.contains("Arabic"))
  
  // 测试资源预加载
  preload_common_resources(manager, [English, Chinese, Spanish, French])
  let preloaded_count = get_preloaded_resource_count(manager)
  assert_true(preloaded_count >= 4)
}

// 测试7: 时区和夏令时处理
test "时区和夏令时处理测试" {
  // 创建UTC时间
  let utc_time = create_utc_date_time(2023, 6, 15, 12, 0, 0) // 夏季
  
  // 转换到不同时区
  let est_time = convert_to_timezone(utc_time, "America/New_York")
  let pst_time = convert_to_timezone(utc_time, "America/Los_Angeles")
  let cet_time = convert_to_timezone(utc_time, "Europe/Paris")
  let jst_time = convert_to_timezone(utc_time, "Asia/Tokyo")
  let cst_time = convert_to_timezone(utc_time, "Asia/Shanghai")
  
  // 验证时区转换（考虑夏令时）
  assert_eq(est_time.hour, 8) // EDT (UTC-4) in summer
  assert_eq(pst_time.hour, 5) // PDT (UTC-7) in summer
  assert_eq(cet_time.hour, 14) // CEST (UTC+2) in summer
  assert_eq(jst_time.hour, 21) // JST (UTC+9)
  assert_eq(cst_time.hour, 20) // CST (UTC+8)
  
  // 测试冬季时间（夏令时不生效）
  let winter_utc_time = create_utc_date_time(2023, 12, 15, 12, 0, 0)
  
  let winter_est_time = convert_to_timezone(winter_utc_time, "America/New_York")
  let winter_pst_time = convert_to_timezone(winter_utc_time, "America/Los_Angeles")
  let winter_cet_time = convert_to_timezone(winter_utc_time, "Europe/Paris")
  
  assert_eq(winter_est_time.hour, 7) // EST (UTC-5) in winter
  assert_eq(winter_pst_time.hour, 4) // PST (UTC-8) in winter
  assert_eq(winter_cet_time.hour, 13) // CET (UTC+1) in winter
  
  // 测试时区缩写显示
  let est_abbrev = get_timezone_abbreviation("America/New_York", utc_time)
  let pst_abbrev = get_timezone_abbreviation("America/Los_Angeles", utc_time)
  let cet_abbrev = get_timezone_abbreviation("Europe/Paris", utc_time)
  
  assert_eq(est_abbrev, "EDT")
  assert_eq(pst_abbrev, "PDT")
  assert_eq(cet_abbrev, "CEST")
  
  // 测试夏令时转换时间
  let dst_transition_spring = create_utc_date_time(2023, 3, 12, 7, 0, 0) // US DST start
  let dst_transition_fall = create_utc_date_time(2023, 11, 5, 6, 0, 0) // US DST end
  
  let before_spring_dst = convert_to_timezone(dst_transition_spring, "America/New_York")
  let after_spring_dst = convert_to_timezone(
    create_utc_date_time(2023, 3, 12, 8, 0, 0), 
    "America/New_York"
  )
  
  let before_fall_dst = convert_to_timezone(dst_transition_fall, "America/New_York")
  let after_fall_dst = convert_to_timezone(
    create_utc_date_time(2023, 11, 5, 7, 0, 0), 
    "America/New_York"
  )
  
  // 验证夏令时转换
  assert_eq(before_spring_dst.hour, 2) // Before DST change
  assert_eq(after_spring_dst.hour, 4) // After DST change (skipped 3 AM)
  
  assert_eq(before_fall_dst.hour, 2) // Before DST change
  assert_eq(after_fall_dst.hour, 2) // After DST change (repeated 1 AM)
}

// 测试8: 文化特定的格式和规则
test "文化特定的格式和规则测试" {
  // 测试姓名格式
  let western_name = format_name("John", "Doe", "Mr.", UnitedStates)
  let eastern_name = format_name("三", "张", "", China)
  let spanish_name = format_name("Juan", "García", "Sr.", Spain)
  
  assert_eq(western_name, "Mr. John Doe")
  assert_eq(eastern_name, "张三")
  assert_eq(spanish_name, "Sr. Juan García")
  
  // 测试地址格式
  let us_address = format_address({
    street: "123 Main St",
    city: "New York",
    state: "NY",
    postal_code: "10001",
    country: "USA"
  }, UnitedStates)
  
  let uk_address = format_address({
    street: "123 Main St",
    city: "London",
    state: "",
    postal_code: "SW1A 1AA",
    country: "UK"
  }, UnitedKingdom)
  
  let japanese_address = format_address({
    street: "Main Street 123",
    city: "Tokyo",
    state: "",
    postal_code: "100-0001",
    country: "Japan"
  }, Japan)
  
  assert_eq(us_address, "123 Main St\nNew York, NY 10001\nUSA")
  assert_eq(uk_address, "123 Main St\nLondon\nSW1A 1AA\nUK")
  assert_eq(japanese_address, "〒100-0001\nTokyo\nMain Street 123\nJapan")
  
  // 测试电话号码格式
  let us_phone = format_phone_number("+12125551234", UnitedStates)
  let uk_phone = format_phone_number("+442071838750", UnitedKingdom)
  let chinese_phone = format_phone_number("+8613812345678", China)
  
  assert_eq(us_phone, "(212) 555-1234")
  assert_eq(uk_phone, "+44 20 7183 8750")
  assert_eq(chinese_phone, "138-1234-5678")
  
  // 测试度量衡单位
  let weight = 70.5 // kg
  
  let us_weight = convert_weight(weight, "kg", "lbs", UnitedStates)
  let uk_weight = convert_weight(weight, "kg", "stone", UnitedKingdom)
  let metric_weight = convert_weight(weight, "kg", "kg", China)
  
  assert_eq(us_weight, "155.4 lbs")
  assert_eq(uk_weight, "11.1 stone")
  assert_eq(metric_weight, "70.5 kg")
  
  let length = 100 // km
  
  let us_length = convert_length(length, "km", "miles", UnitedStates)
  let metric_length = convert_length(length, "km", "km", China)
  
  assert_eq(us_length, "62.1 miles")
  assert_eq(metric_length, "100 km")
}

// 辅助函数：获取本地化文本
fn get_localized_text(key : String, language : Language) -> String {
  // 简化的本地化文本获取
  // 在实际项目中，会从资源文件中加载
  match (key, language) {
    ("welcome_message", English) => "Welcome to our application!"
    ("welcome_message", Chinese) => "欢迎使用我们的应用程序！"
    ("welcome_message", Spanish) => "¡Bienvenido a nuestra aplicación!"
    ("welcome_message", Arabic) => "مرحبا بكم في تطبيقنا!"
    ("user_greeting", English) => "Hello, {name}!"
    ("user_greeting", Chinese) => "你好，{name}！"
    ("item_count", English) => if 1 == 1 { "You have 1 item" } else { "You have {count} items" }
    ("item_count", Chinese) => "您有{count}个项目"
    ("user_title", English) => match "" {
      "male" => "Mr."
      "female" => "Ms."
      _ => "Mx."
    }
    _ => "Text not found"
  }
}

// 辅助函数：获取参数化文本
fn get_parameterized_text(key : String, language : Language, parameters : Array[(String, String)]) -> String {
  let template = get_localized_text(key, language)
  let result = template
  
  for param in parameters {
    let placeholder = "{" + param.0 + "}"
    result = result.replace(placeholder, param.1)
  }
  
  result
}

// 辅助函数：获取复数化文本
fn get_pluralized_text(key : String, language : Language, count : Int) -> String {
  match language {
    English => {
      if count == 1 {
        get_parameterized_text(key, language, [("count", Int.to_string(count))])
      } else {
        get_parameterized_text(key + "_plural", language, [("count", Int.to_string(count))])
      }
    }
    Chinese => {
      // 中文通常不区分单复数
      get_parameterized_text(key, language, [("count", Int.to_string(count))])
    }
    _ => get_parameterized_text(key, language, [("count", Int.to_string(count))])
  }
}

// 辅助函数：获取性别化文本
fn get_gendered_text(key : String, language : Language, gender : String) -> String {
  match (key, language, gender) {
    ("user_title", English, "male") => "Mr."
    ("user_title", English, "female") => "Ms."
    ("user_title", English, _) => "Mx."
    _ => "Title"
  }
}

// 日期时间类型
type DateTime = {
  year : Int,
  month : Int,
  day : Int,
  hour : Int,
  minute : Int,
  second : Int
}

// 辅助函数：创建日期时间
fn create_date_time(year : Int, month : Int, day : Int, hour : Int, minute : Int, second : Int) -> DateTime {
  { year: year, month: month, day: day, hour: hour, minute: minute, second: second }
}

// 辅助函数：创建UTC日期时间
fn create_utc_date_time(year : Int, month : Int, day : Int, hour : Int, minute : Int, second : Int) -> DateTime {
  create_date_time(year, month, day, hour, minute, second)
}

// 辅助函数：格式化日期
fn format_date(date : DateTime, region : Region) -> String {
  match region {
    UnitedStates => Int.to_string(date.month) + "/" + Int.to_string(date.day) + "/" + Int.to_string(date.year)
    UnitedKingdom => pad_zero(date.day) + "/" + pad_zero(date.month) + "/" + Int.to_string(date.year)
    China => Int.to_string(date.year) + "年" + Int.to_string(date.month) + "月" + Int.to_string(date.day) + "日"
    Japan => Int.to_string(date.year) + "/" + pad_zero(date.month) + "/" + pad_zero(date.day)
    _ => Int.to_string(date.year) + "-" + pad_zero(date.month) + "-" + pad_zero(date.day)
  }
}

// 辅助函数：格式化时间
fn format_time(date : DateTime, region : Region) -> String {
  match region {
    UnitedStates => {
      let hour = date.hour % 12
      if hour == 0 { hour = 12 }
      let period = if date.hour < 12 { "AM" } else { "PM" }
      Int.to_string(hour) + ":" + pad_zero(date.minute) + ":" + pad_zero(date.second) + " " + period
    }
    UnitedKingdom => pad_zero(date.hour) + ":" + pad_zero(date.minute) + ":" + pad_zero(date.second)
    China => {
      let period = if date.hour < 12 { "上午" } else { "下午" }
      let hour = if date.hour > 12 { date.hour - 12 } else { date.hour }
      period + Int.to_string(hour) + ":" + pad_zero(date.minute) + ":" + pad_zero(date.second)
    }
    Japan => pad_zero(date.hour) + ":" + pad_zero(date.minute) + ":" + pad_zero(date.second)
    _ => pad_zero(date.hour) + ":" + pad_zero(date.minute) + ":" + pad_zero(date.second)
  }
}

// 辅助函数：格式化日期时间
fn format_date_time(date : DateTime, region : Region) -> String {
  format_date(date, region) + ", " + format_time(date, region)
}

// 辅助函数：格式化相对时间
fn format_relative_time(from_date : DateTime, to_date : DateTime, language : Language) -> String {
  let diff_seconds = calculate_time_difference(from_date, to_date)
  let abs_diff = if diff_seconds < 0 { -diff_seconds } else { diff_seconds }
  
  let (value, unit) = if abs_diff < 60 {
    (abs_diff, "second")
  } else if abs_diff < 3600 {
    (abs_diff / 60, "minute")
  } else if abs_diff < 86400 {
    (abs_diff / 3600, "hour")
  } else {
    (abs_diff / 86400, "day")
  }
  
  let unit_text = if value == 1 { unit } else { unit + "s" }
  
  match language {
    English => {
      if diff_seconds >= 0 {
        "in " + Int.to_string(value) + " " + unit_text
      } else {
        Int.to_string(value) + " " + unit_text + " ago"
      }
    }
    Chinese => {
      if diff_seconds >= 0 {
        Int.to_string(value) + unit_text.replace("second", "秒").replace("minute", "分钟").replace("hour", "小时").replace("day", "天") + "后"
      } else {
        Int.to_string(value) + unit_text.replace("second", "秒").replace("minute", "分钟").replace("hour", "小时").replace("day", "天") + "前"
      }
    }
    _ => {
      if diff_seconds >= 0 {
        "in " + Int.to_string(value) + " " + unit_text
      } else {
        Int.to_string(value) + " " + unit_text + " ago"
      }
    }
  }
}

// 辅助函数：计算时间差（秒）
fn calculate_time_difference(from_date : DateTime, to_date : DateTime) -> Int {
  // 简化的时间差计算
  let from_total = from_date.year * 365 * 24 * 3600 + 
                  from_date.month * 30 * 24 * 3600 + 
                  from_date.day * 24 * 3600 + 
                  from_date.hour * 3600 + 
                  from_date.minute * 60 + 
                  from_date.second
                  
  let to_total = to_date.year * 365 * 24 * 3600 + 
                to_date.month * 30 * 24 * 3600 + 
                to_date.day * 24 * 3600 + 
                to_date.hour * 3600 + 
                to_date.minute * 60 + 
                to_date.second
                
  to_total - from_total
}

// 辅助函数：补零
fn pad_zero(num : Int) -> String {
  if num < 10 {
    "0" + Int.to_string(num)
  } else {
    Int.to_string(num)
  }
}

// 辅助函数：格式化数字
fn format_number(number : Float, region : Region) -> String {
  // 简化的数字格式化
  // 在实际项目中，会使用更复杂的本地化库
  let int_part = number as Int
  let decimal_part = ((number - int_part as Float) * 100) as Int
  
  let int_str = format_integer_with_grouping(int_part, region)
  let decimal_str = pad_zero(decimal_part)
  
  match region {
    Germany | France => int_str + "," + decimal_str
    _ => int_str + "." + decimal_str
  }
}

// 辅助函数：格式化整数分组
fn format_integer_with_grouping(number : Int, region : Region) -> String {
  let num_str = Int.to_string(number)
  let groups = []
  let i = num_str.length()
  
  while i > 0 {
    let start = if i > 3 { i - 3 } else { 0 }
    groups.push(num_str.substring(start, i - start))
    i = start
  }
  
  let separator = match region {
    Germany => "."
    France => " "
    _ => ","
  }
  
  let reversed = groups.reverse()
  String.join(separator, reversed)
}

// 辅助函数：格式化百分比
fn format_percentage(value : Float, region : Region) -> String {
  let percentage = value * 100
  let formatted = format_number(percentage, region)
  
  // 移除多余的小数位
  let trimmed = if formatted.contains(".") {
    let parts = formatted.split(".")
    parts[0] + "." + parts[1].substring(0, 2)
  } else {
    formatted
  }
  
  trimmed + "%"
}

// 辅助函数：格式化货币
fn format_currency(amount : Float, currency : String, region : Region) -> String {
  let formatted_number = format_number(amount, region)
  
  match (currency, region) {
    ("USD", UnitedStates) => "$" + formatted_number
    ("EUR", Germany) => formatted_number + " €"
    ("GBP", UnitedKingdom) => "£" + formatted_number
    ("CNY", China) => "¥" + formatted_number
    ("JPY", Japan) => "¥" + Int.to_string(amount as Int) // 日元不显示小数
    _ => currency + " " + formatted_number
  }
}

// 辅助函数：格式化大数字
fn format_large_number(number : Int, region : Region) -> String {
  match region {
    UnitedStates => {
      if number >= 1000000000 {
        Float.to_string((number as Float) / 1000000000) + "B"
      } else if number >= 1000000 {
        Float.to_string((number as Float) / 1000000) + "M"
      } else if number >= 1000 {
        Float.to_string((number as Float) / 1000) + "K"
      } else {
        Int.to_string(number)
      }
    }
    China => {
      if number >= 100000000 {
        Float.to_string((number as Float) / 100000000) + "亿"
      } else if number >= 10000 {
        Float.to_string((number as Float) / 10000) + "万"
      } else {
        Int.to_string(number)
      }
    }
    _ => Int.to_string(number)
  }
}

// 辅助函数：获取文本方向
fn get_text_direction(language : Language) -> TextDirection {
  match language {
    Arabic => RightToLeft
    Chinese => LeftToRight
    Japanese => TopToBottom
    _ => LeftToRight
  }
}

// 辅助函数：处理混合方向文本
fn process_mixed_direction_text(text : String) -> String {
  // 简化的混合方向文本处理
  // 在实际项目中，会使用Unicode双向算法
  text
}

// 辅助函数：获取布局对齐
fn get_layout_alignment(language : Language) -> String {
  match get_text_direction(language) {
    LeftToRight => "left"
    RightToLeft => "right"
    TopToBottom => "center"
  }
}

// UI元素类型
type UIElements = {
  back_button : String,
  next_button : String,
  navigation : String
}

// 辅助函数：镜像UI元素
fn mirror_ui_elements(ui : UIElements, direction : TextDirection) -> UIElements {
  match direction {
    RightToLeft => {
      {
        back_button: ui.back_button.replace("←", "→").replace("Back", "Back →"),
        next_button: ui.next_button.replace("→", "←").replace("Next", "← Next"),
        navigation: String.join(" | ", ui.navigation.split(" | ").reverse())
      }
    }
    _ => ui
  }
}

// 辅助函数：检查是否为有效Unicode
fn is_valid_unicode(text : String) -> Bool {
  // 简化的Unicode验证
  // 在实际项目中，会使用更严格的Unicode验证
  text.to_utf8_bytes().length() > 0
}

// 辅助函数：计算Unicode字符数
fn count_unicode_characters(text : String) -> Int {
  // 简化的Unicode字符计数
  // 在实际项目中，会考虑组合字符和代理对
  text.length()
}

// 辅助函数：搜索文本
fn search_text(text : String, query : String) -> Bool {
  text.contains(query)
}

// 本地化管理器类型
type LocalizationManager = {
  active_language : Language,
  loaded_resources : Array[(Language, Array[(String, String)])],
  cached_resources : Array[String]
}

// 辅助函数：创建本地化管理器
fn create_localization_manager() -> LocalizationManager {
  {
    active_language: English,
    loaded_resources: [],
    cached_resources: []
  }
}

// 辅助函数：加载语言资源
fn load_language_resources(manager : LocalizationManager, language : Language) -> Unit {
  // 简化的资源加载
  // 在实际项目中，会从文件或网络加载
  let resources = match language {
    English => [("submit_button", "Submit"), ("cancel_button", "Cancel")]
    Chinese => [("submit_button", "提交"), ("cancel_button", "取消")]
    Arabic => [("submit_button", "إرسال"), ("cancel_button", "إلغاء")]
    _ => [("submit_button", "Submit"), ("cancel_button", "Cancel")]
  }
  
  manager.loaded_resources.push((language, resources))
  
  if !manager.cached_resources.contains(language.to_string()) {
    manager.cached_resources.push(language.to_string())
  }
}

// 辅助函数：设置活动语言
fn set_active_language(manager : LocalizationManager, language : Language) -> Unit {
  manager.active_language = language
}

// 辅助函数：获取UI文本
fn get_ui_text(manager : LocalizationManager, key : String) -> String {
  // 查找当前语言的资源
  let i = 0
  while i < manager.loaded_resources.length() {
    let (lang, resources) = manager.loaded_resources[i]
    if lang == manager.active_language {
      let j = 0
      while j < resources.length() {
        if resources[j].0 == key {
          return resources[j].1
        }
        j = j + 1
      }
    }
    i = i + 1
  }
  
  // 回退到英语
  let i = 0
  while i < manager.loaded_resources.length() {
    let (lang, resources) = manager.loaded_resources[i]
    if lang == English {
      let j = 0
      while j < resources.length() {
        if resources[j].0 == key {
          return resources[j].1
        }
        j = j + 1
      }
    }
    i = i + 1
  }
  
  "Text not found"
}

// 辅助函数：更新语言资源
fn update_language_resource(manager : LocalizationManager, language : Language, key : String, value : String) -> Unit {
  let i = 0
  while i < manager.loaded_resources.length() {
    let (lang, resources) = manager.loaded_resources[i]
    if lang == language {
      let j = 0
      while j < resources.length() {
        if resources[j].0 == key {
          resources[j] = (key, value)
          return
        }
        j = j + 1
      }
      // 如果键不存在，添加新键值对
      resources.push((key, value))
      return
    }
    i = i + 1
  }
}

// 辅助函数：获取缓存的资源
fn get_cached_resources(manager : LocalizationManager) -> Array[String] {
  manager.cached_resources
}

// 辅助函数：预加载常用资源
fn preload_common_resources(manager : LocalizationManager, languages : Array[Language]) -> Unit {
  for language in languages {
    load_language_resources(manager, language)
  }
}

// 辅助函数：获取预加载的资源数量
fn get_preloaded_resource_count(manager : LocalizationManager) -> Int {
  manager.loaded_resources.length()
}

// 辅助函数：转换时区
fn convert_to_timezone(date : DateTime, timezone : String) -> DateTime {
  // 简化的时区转换
  // 在实际项目中，会使用时区数据库
  let offset_hours = match timezone {
    "America/New_York" => if date.month >= 3 && date.month < 11 { -4 } else { -5 }
    "America/Los_Angeles" => if date.month >= 3 && date.month < 11 { -7 } else { -8 }
    "Europe/Paris" => if date.month >= 3 && date.month < 11 { 2 } else { 1 }
    "Asia/Tokyo" => 9
    "Asia/Shanghai" => 8
    _ => 0
  }
  
  let new_hour = date.hour + offset_hours
  let adjusted_hour = if new_hour < 0 { new_hour + 24 } else if new_hour >= 24 { new_hour - 24 } else { new_hour }
  
  { date with hour: adjusted_hour }
}

// 辅助函数：获取时区缩写
fn get_timezone_abbreviation(timezone : String, date : DateTime) -> String {
  match timezone {
    "America/New_York" => if date.month >= 3 && date.month < 11 { "EDT" } else { "EST" }
    "America/Los_Angeles" => if date.month >= 3 && date.month < 11 { "PDT" } else { "PST" }
    "Europe/Paris" => if date.month >= 3 && date.month < 11 { "CEST" } else { "CET" }
    _ => "UTC"
  }
}

// 地址类型
type Address = {
  street : String,
  city : String,
  state : String,
  postal_code : String,
  country : String
}

// 辅助函数：格式化姓名
fn format_name(first_name : String, last_name : String, title : String, region : Region) -> String {
  match region {
    China => last_name + first_name
    _ => {
      if title.length() > 0 {
        title + " " + first_name + " " + last_name
      } else {
        first_name + " " + last_name
      }
    }
  }
}

// 辅助函数：格式化地址
fn format_address(address : Address, region : Region) -> String {
  match region {
    UnitedStates => {
      let state_part = if address.state.length() > 0 {
        address.city + ", " + address.state + " " + address.postal_code
      } else {
        address.city + " " + address.postal_code
      }
      
      address.street + "\n" + state_part + "\n" + address.country
    }
    Japan => {
      "〒" + address.postal_code + "\n" + 
      address.city + "\n" + 
      address.street + "\n" + 
      address.country
    }
    _ => {
      address.street + "\n" + 
      address.city + "\n" + 
      address.postal_code + "\n" + 
      address.country
    }
  }
}

// 辅助函数：格式化电话号码
fn format_phone_number(phone : String, region : Region) -> String {
  match region {
    UnitedStates => {
      if phone.length() >= 10 && phone.starts_with("+1") {
        let area = phone.substring(2, 3)
        let prefix = phone.substring(5, 3)
        let line = phone.substring(8, 4)
        "(" + area + ") " + prefix + "-" + line
      } else {
        phone
      }
    }
    UnitedKingdom => {
      if phone.length() >= 12 && phone.starts_with("+44") {
        let area = phone.substring(3, 2)
        let rest = phone.substring(5, phone.length() - 5)
        "+44 " + area + " " + rest
      } else {
        phone
      }
    }
    China => {
      if phone.length() >= 13 && phone.starts_with("+86") {
        let prefix = phone.substring(3, 3)
        let line1 = phone.substring(6, 4)
        let line2 = phone.substring(10, phone.length() - 10)
        prefix + "-" + line1 + "-" + line2
      } else {
        phone
      }
    }
    _ => phone
  }
}

// 辅助函数：转换重量
fn convert_weight(value : Float, from_unit : String, to_unit : String, region : Region) -> String {
  let converted_value = if from_unit == "kg" && to_unit == "lbs" {
    value * 2.20462
  } else if from_unit == "kg" && to_unit == "stone" {
    value / 6.35029
  } else {
    value
  }
  
  Float.to_string(converted_value) + " " + to_unit
}

// 辅助函数：转换长度
fn convert_length(value : Int, from_unit : String, to_unit : String, region : Region) -> String {
  let converted_value = if from_unit == "km" && to_unit == "miles" {
    value as Float * 0.621371
  } else {
    value as Float
  }
  
  Float.to_string(converted_value) + " " + to_unit
}