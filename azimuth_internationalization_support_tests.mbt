// Azimuth 国际化支持测试用例
// 测试系统在不同语言、地区和文化设置下的本地化功能

test "多语言消息和错误处理" {
  // 创建国际化管理器
  let i18n_manager = azimuth::I18nManager::new()
  
  // 加载语言资源
  i18n_manager.load_language_resource("zh-CN", [
    ("error.network_timeout", "网络连接超时"),
    ("error.database_failed", "数据库连接失败"),
    ("error.invalid_input", "输入数据无效"),
    ("message.operation_success", "操作成功完成"),
    ("message.user_created", "用户 {username} 已创建"),
    ("message.order_processed", "订单 {order_id} 已处理")
  ])
  
  i18n_manager.load_language_resource("en-US", [
    ("error.network_timeout", "Network connection timeout"),
    ("error.database_failed", "Database connection failed"),
    ("error.invalid_input", "Invalid input data"),
    ("message.operation_success", "Operation completed successfully"),
    ("message.user_created", "User {username} has been created"),
    ("message.order_processed", "Order {order_id} has been processed")
  ])
  
  i18n_manager.load_language_resource("ja-JP", [
    ("error.network_timeout", "ネットワーク接続タイムアウト"),
    ("error.database_failed", "データベース接続に失敗しました"),
    ("error.invalid_input", "入力データが無効です"),
    ("message.operation_success", "操作が正常に完了しました"),
    ("message.user_created", "ユーザー {username} が作成されました"),
    ("message.order_processed", "注文 {order_id} が処理されました")
  ])
  
  // 设置当前语言为中文
  i18n_manager.set_current_language("zh-CN")
  
  // 测试错误消息本地化
  let network_error = azimuth::Error::new("NetworkTimeout", "Connection timeout")
  let localized_network_error = i18n_manager.localize_error(network_error)
  assert_eq(localized_network_error.get_message(), "网络连接超时")
  
  let db_error = azimuth::Error::new("DatabaseFailed", "Cannot connect to database")
  let localized_db_error = i18n_manager.localize_error(db_error)
  assert_eq(localized_db_error.get_message(), "数据库连接失败")
  
  // 测试参数化消息
  let user_message = i18n_manager.get_message("message.user_created", [
    ("username", "张三")
  ])
  assert_eq(user_message, "用户 张三 已创建")
  
  let order_message = i18n_manager.get_message("message.order_processed", [
    ("order_id", "ORD-12345")
  ])
  assert_eq(order_message, "订单 ORD-12345 已处理")
  
  // 切换到英语
  i18n_manager.set_current_language("en-US")
  
  // 测试英语本地化
  let en_network_error = i18n_manager.localize_error(network_error)
  assert_eq(en_network_error.get_message(), "Network connection timeout")
  
  let en_user_message = i18n_manager.get_message("message.user_created", [
    ("username", "John Doe")
  ])
  assert_eq(en_user_message, "User John Doe has been created")
  
  // 切换到日语
  i18n_manager.set_current_language("ja-JP")
  
  // 测试日语本地化
  let ja_network_error = i18n_manager.localize_error(network_error)
  assert_eq(ja_network_error.get_message(), "ネットワーク接続タイムアウト")
  
  let ja_user_message = i18n_manager.get_message("message.user_created", [
    ("username", "田中太郎")
  ])
  assert_eq(ja_user_message, "ユーザー 田中太郎 が作成されました")
  
  // 测试回退机制（当消息不存在时）
  i18n_manager.set_current_language("zh-CN")
  let missing_message = i18n_manager.get_message("message.nonexistent", [])
  assert_eq(missing_message, "message.nonexistent") // 回退到消息键
}

test "日期时间格式本地化" {
  // 创建日期时间本地化器
  let datetime_localizer = azimuth::DateTimeLocalizer::new()
  
  // 配置不同地区的日期时间格式
  datetime_localizer.set_locale_format("zh-CN", azimuth::DateTimeFormat {
    date_format: "yyyy年MM月dd日",
    time_format: "HH:mm:ss",
    datetime_format: "yyyy年MM月dd日 HH:mm:ss",
    timezone: "Asia/Shanghai"
  })
  
  datetime_localizer.set_locale_format("en-US", azimuth::DateTimeFormat {
    date_format: "MM/dd/yyyy",
    time_format: "hh:mm:ss a",
    datetime_format: "MM/dd/yyyy hh:mm:ss a",
    timezone: "America/New_York"
  })
  
  datetime_localizer.set_locale_format("ja-JP", azimuth::DateTimeFormat {
    date_format: "yyyy/MM/dd",
    time_format: "HH:mm:ss",
    datetime_format: "yyyy/MM/dd HH:mm:ss",
    timezone: "Asia/Tokyo"
  })
  
  // 创建测试时间戳
  let timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // 测试中文格式
  datetime_localizer.set_current_locale("zh-CN")
  let zh_datetime = datetime_localizer.format_datetime(timestamp)
  assert_eq(zh_datetime, "2021年01月01日 08:00:00") // 北京时间 UTC+8
  
  let zh_date = datetime_localizer.format_date(timestamp)
  assert_eq(zh_date, "2021年01月01日")
  
  let zh_time = datetime_localizer.format_time(timestamp)
  assert_eq(zh_time, "08:00:00")
  
  // 测试英语格式
  datetime_localizer.set_current_locale("en-US")
  let en_datetime = datetime_localizer.format_datetime(timestamp)
  assert_eq(en_datetime, "12/31/2020 07:00:00 PM") // 纽约时间 UTC-5
  
  let en_date = datetime_localizer.format_date(timestamp)
  assert_eq(en_date, "12/31/2020")
  
  let en_time = datetime_localizer.format_time(timestamp)
  assert_eq(en_time, "07:00:00 PM")
  
  // 测试日语格式
  datetime_localizer.set_current_locale("ja-JP")
  let ja_datetime = datetime_localizer.format_datetime(timestamp)
  assert_eq(ja_datetime, "2021/01/01 09:00:00") // 东京时间 UTC+9
  
  let ja_date = datetime_localizer.format_date(timestamp)
  assert_eq(ja_date, "2021/01/01")
  
  let ja_time = datetime_localizer.format_time(timestamp)
  assert_eq(ja_time, "09:00:00")
  
  // 测试相对时间格式
  let current_time = 1609545600000L // 2021-01-02 00:00:00 UTC
  let past_time = 1609459200000L    // 2021-01-01 00:00:00 UTC (1天前)
  
  datetime_localizer.set_current_locale("zh-CN")
  let zh_relative = datetime_localizer.format_relative_time(past_time, current_time)
  assert_eq(zh_relative, "1天前")
  
  datetime_localizer.set_current_locale("en-US")
  let en_relative = datetime_localizer.format_relative_time(past_time, current_time)
  assert_eq(en_relative, "1 day ago")
  
  datetime_localizer.set_current_locale("ja-JP")
  let ja_relative = datetime_localizer.format_relative_time(past_time, current_time)
  assert_eq(ja_relative, "1日前")
}

test "数字和货币格式本地化" {
  // 创建数字格式化器
  let number_formatter = azimuth::NumberFormatter::new()
  
  // 配置不同地区的数字格式
  number_formatter.set_locale_format("zh-CN", azimuth::NumberFormat {
    decimal_separator: ".",
    thousands_separator: ",",
    currency_symbol: "¥",
    currency_format: "¥#,##0.00",
    percentage_format: "#,##0.00%"
  })
  
  number_formatter.set_locale_format("en-US", azimuth::NumberFormat {
    decimal_separator: ".",
    thousands_separator: ",",
    currency_symbol: "$",
    currency_format: "$#,##0.00",
    percentage_format: "#,##0.00%"
  })
  
  number_formatter.set_locale_format("ja-JP", azimuth::NumberFormat {
    decimal_separator: ".",
    thousands_separator: ",",
    currency_symbol: "¥",
    currency_format: "¥#,##0",
    percentage_format: "#,##0.00%"
  })
  
  number_formatter.set_locale_format("de-DE", azimuth::NumberFormat {
    decimal_separator: ",",
    thousands_separator: ".",
    currency_symbol: "€",
    currency_format: "#.##0,00 €",
    percentage_format: "#.##0,00%"
  })
  
  // 测试数字格式化
  let test_number = 1234567.89
  
  number_formatter.set_current_locale("zh-CN")
  let zh_number = number_formatter.format_number(test_number)
  assert_eq(zh_number, "1,234,567.89")
  
  number_formatter.set_current_locale("en-US")
  let en_number = number_formatter.format_number(test_number)
  assert_eq(en_number, "1,234,567.89")
  
  number_formatter.set_current_locale("de-DE")
  let de_number = number_formatter.format_number(test_number)
  assert_eq(de_number, "1.234.567,89")
  
  // 测试货币格式化
  number_formatter.set_current_locale("zh-CN")
  let zh_currency = number_formatter.format_currency(test_number)
  assert_eq(zh_currency, "¥1,234,567.89")
  
  number_formatter.set_current_locale("en-US")
  let en_currency = number_formatter.format_currency(test_number)
  assert_eq(en_currency, "$1,234,567.89")
  
  number_formatter.set_current_locale("ja-JP")
  let ja_currency = number_formatter.format_currency(test_number)
  assert_eq(ja_currency, "¥1,234,568") // 日语通常不显示小数
  
  number_formatter.set_current_locale("de-DE")
  let de_currency = number_formatter.format_currency(test_number)
  assert_eq(de_currency, "1.234.567,89 €")
  
  // 测试百分比格式化
  let test_percentage = 0.1234
  
  number_formatter.set_current_locale("zh-CN")
  let zh_percentage = number_formatter.format_percentage(test_percentage)
  assert_eq(zh_percentage, "12.34%")
  
  number_formatter.set_current_locale("de-DE")
  let de_percentage = number_formatter.format_percentage(test_percentage)
  assert_eq(de_percentage, "12,34%")
}

test "文本排序和搜索本地化" {
  // 创建文本本地化器
  let text_localizer = azimuth::TextLocalizer::new()
  
  // 配置不同地区的排序规则
  text_localizer.set_locale_collation("zh-CN", azimuth::Collation {
    language: "zh",
    country: "CN",
    strength: azimuth::CollationStrength::Tertiary, // 区分大小写和重音
    case_level: false,
    numeric_ordering: true
  })
  
  text_localizer.set_locale_collation("en-US", azimuth::Collation {
    language: "en",
    country: "US",
    strength: azimuth::CollationStrength::Secondary, // 忽略大小写差异
    case_level: true,
    numeric_ordering: true
  })
  
  text_localizer.set_locale_collation("ja-JP", azimuth::Collation {
    language: "ja",
    country: "JP",
    strength: azimuth::CollationStrength::Primary, // 只区分基本字符
    case_level: false,
    numeric_ordering: true
  })
  
  // 测试中文排序
  text_localizer.set_current_locale("zh-CN")
  let chinese_names = ["张三", "李四", "王五", "赵六", "钱七", "孙八"]
  let sorted_chinese = text_localizer.sort_strings(chinese_names)
  assert_eq(sorted_chinese, ["钱七", "李四", "孙八", "王五", "张三", "赵六"])
  
  // 测试英语排序
  text_localizer.set_current_locale("en-US")
  let english_names = ["alice", "Bob", "charlie", "David", "Eve", "frank"]
  let sorted_english = text_localizer.sort_strings(english_names)
  assert_eq(sorted_english, ["alice", "Bob", "charlie", "David", "Eve", "frank"])
  
  // 测试日语排序
  text_localizer.set_current_locale("ja-JP")
  let japanese_names = ["あいう", "かきく", "さしす", "たちつ", "なにぬ"]
  let sorted_japanese = text_localizer.sort_strings(japanese_names)
  assert_eq(sorted_japanese, ["あいう", "かきく", "さしす", "たちつ", "なにぬ"])
  
  // 测试搜索本地化
  text_localizer.set_current_locale("zh-CN")
  let chinese_text = "这是一个测试文本，包含中文字符。"
  let chinese_search_result = text_localizer.search_text(chinese_text, "测试")
  assert_true(chinese_search_result.found)
  assert_eq(chinese_search_result.start_index, 4)
  assert_eq(chinese_search_result.end_index, 6)
  
  // 测试不区分大小写的搜索
  text_localizer.set_current_locale("en-US")
  let english_text = "This is a Test Text with Mixed CASE."
  let case_insensitive_search = text_localizer.search_text(english_text, "test")
  assert_true(case_insensitive_search.found)
  assert_eq(case_insensitive_search.start_index, 10)
  assert_eq(case_insensitive_search.end_index, 14)
}

test "时区和夏令时处理" {
  // 创建时区处理器
  let timezone_handler = azimuth::TimezoneHandler::new()
  
  // 测试不同时区的时间转换
  let utc_timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // 转换到北京时间 (UTC+8)
  let beijing_time = timezone_handler.convert_timezone(utc_timestamp, "UTC", "Asia/Shanghai")
  assert_eq(beijing_time.year, 2021)
  assert_eq(beijing_time.month, 1)
  assert_eq(beijing_time.day, 1)
  assert_eq(beijing_time.hour, 8)
  assert_eq(beijing_time.minute, 0)
  assert_eq(beijing_time.second, 0)
  
  // 转换到纽约时间 (UTC-5，不考虑夏令时)
  let new_york_time = timezone_handler.convert_timezone(utc_timestamp, "UTC", "America/New_York")
  assert_eq(new_york_time.year, 2020)
  assert_eq(new_york_time.month, 12)
  assert_eq(new_york_time.day, 31)
  assert_eq(new_york_time.hour, 19)
  assert_eq(new_york_time.minute, 0)
  assert_eq(new_york_time.second, 0)
  
  // 转换到东京时间 (UTC+9)
  let tokyo_time = timezone_handler.convert_timezone(utc_timestamp, "UTC", "Asia/Tokyo")
  assert_eq(tokyo_time.year, 2021)
  assert_eq(tokyo_time.month, 1)
  assert_eq(tokyo_time.day, 1)
  assert_eq(tokyo_time.hour, 9)
  assert_eq(tokyo_time.minute, 0)
  assert_eq(tokyo_time.second, 0)
  
  // 测试夏令时
  // 2021年7月1日中午12点 UTC (纽约夏令时期间，UTC-4)
  let summer_timestamp = 1625133600000L // 2021-07-01 12:00:00 UTC
  let summer_new_york_time = timezone_handler.convert_timezone(summer_timestamp, "UTC", "America/New_York")
  assert_eq(summer_new_york_time.year, 2021)
  assert_eq(summer_new_york_time.month, 7)
  assert_eq(summer_new_york_time.day, 1)
  assert_eq(summer_new_york_time.hour, 8) // 夏令时期间是UTC-4，所以12-4=8
  
  // 测试时区偏移计算
  let beijing_offset = timezone_handler.get_timezone_offset("Asia/Shanghai", utc_timestamp)
  assert_eq(beijing_offset, 8 * 60 * 60) // 8小时，单位秒
  
  let new_york_offset = timezone_handler.get_timezone_offset("America/New_York", utc_timestamp)
  assert_eq(new_york_offset, -5 * 60 * 60) // -5小时，单位秒
  
  let summer_new_york_offset = timezone_handler.get_timezone_offset("America/New_York", summer_timestamp)
  assert_eq(summer_new_york_offset, -4 * 60 * 60) // 夏令时期间是-4小时
  
  // 测试时区转换的往返一致性
  let original_utc = utc_timestamp
  let converted_to_beijing = timezone_handler.convert_timezone(original_utc, "UTC", "Asia/Shanghai")
  let converted_back_to_utc = timezone_handler.convert_timezone(
    converted_to_beijing.to_timestamp(), 
    "Asia/Shanghai", 
    "UTC"
  )
  assert_eq(original_utc, converted_back_to_utc.to_timestamp())
}

test "多语言UI组件本地化" {
  // 创建UI本地化器
  let ui_localizer = azimuth::UILocalizer::new()
  
  // 加载UI文本资源
  ui_localizer.load_ui_resource("zh-CN", [
    ("button.save", "保存"),
    ("button.cancel", "取消"),
    ("button.delete", "删除"),
    ("label.username", "用户名"),
    ("label.password", "密码"),
    ("label.email", "电子邮箱"),
    ("menu.file", "文件"),
    ("menu.edit", "编辑"),
    ("menu.view", "视图"),
    ("title.login", "登录"),
    ("title.user_profile", "用户资料"),
    ("error.required_field", "字段 {field_name} 是必填的"),
    ("error.invalid_email", "电子邮箱格式无效")
  ])
  
  ui_localizer.load_ui_resource("en-US", [
    ("button.save", "Save"),
    ("button.cancel", "Cancel"),
    ("button.delete", "Delete"),
    ("label.username", "Username"),
    ("label.password", "Password"),
    ("label.email", "Email"),
    ("menu.file", "File"),
    ("menu.edit", "Edit"),
    ("menu.view", "View"),
    ("title.login", "Login"),
    ("title.user_profile", "User Profile"),
    ("error.required_field", "The field {field_name} is required"),
    ("error.invalid_email", "Invalid email format")
  ])
  
  ui_localizer.load_ui_resource("ja-JP", [
    ("button.save", "保存"),
    ("button.cancel", "キャンセル"),
    ("button.delete", "削除"),
    ("label.username", "ユーザー名"),
    ("label.password", "パスワード"),
    ("label.email", "メールアドレス"),
    ("menu.file", "ファイル"),
    ("menu.edit", "編集"),
    ("menu.view", "表示"),
    ("title.login", "ログイン"),
    ("title.user_profile", "ユーザープロフィール"),
    ("error.required_field", "フィールド {field_name} は必須です"),
    ("error.invalid_email", "メールアドレスの形式が無効です")
  ])
  
  // 创建UI组件
  let login_form = azimuth::UIForm::new("login_form")
  login_form.add_field(azimuth::UIField::new("username", "label.username", "text"))
  login_form.add_field(azimuth::UIField::new("password", "label.password", "password"))
  login_form.add_field(azimuth::UIField::new("email", "label.email", "email"))
  login_form.add_button(azimuth::UIButton::new("save", "button.save"))
  login_form.add_button(azimuth::UIButton::new("cancel", "button.cancel"))
  
  // 本地化为中文
  ui_localizer.set_current_language("zh-CN")
  let zh_localized_form = ui_localizer.localize_form(login_form)
  
  assert_eq(zh_localized_form.get_field("username").label, "用户名")
  assert_eq(zh_localized_form.get_field("password").label, "密码")
  assert_eq(zh_localized_form.get_field("email").label, "电子邮箱")
  assert_eq(zh_localized_form.get_button("save").text, "保存")
  assert_eq(zh_localized_form.get_button("cancel").text, "取消")
  
  // 本地化为英语
  ui_localizer.set_current_language("en-US")
  let en_localized_form = ui_localizer.localize_form(login_form)
  
  assert_eq(en_localized_form.get_field("username").label, "Username")
  assert_eq(en_localized_form.get_field("password").label, "Password")
  assert_eq(en_localized_form.get_field("email").label, "Email")
  assert_eq(en_localized_form.get_button("save").text, "Save")
  assert_eq(en_localized_form.get_button("cancel").text, "Cancel")
  
  // 本地化为日语
  ui_localizer.set_current_language("ja-JP")
  let ja_localized_form = ui_localizer.localize_form(login_form)
  
  assert_eq(ja_localized_form.get_field("username").label, "ユーザー名")
  assert_eq(ja_localized_form.get_field("password").label, "パスワード")
  assert_eq(ja_localized_form.get_field("email").label, "メールアドレス")
  assert_eq(ja_localized_form.get_button("save").text, "保存")
  assert_eq(ja_localized_form.get_button("cancel").text, "キャンセル")
  
  // 测试错误消息本地化
  ui_localizer.set_current_language("zh-CN")
  let zh_error_message = ui_localizer.localize_error_message("error.required_field", [
    ("field_name", "用户名")
  ])
  assert_eq(zh_error_message, "字段 用户名 是必填的")
  
  ui_localizer.set_current_language("en-US")
  let en_error_message = ui_localizer.localize_error_message("error.required_field", [
    ("field_name", "Username")
  ])
  assert_eq(en_error_message, "The field Username is required")
  
  ui_localizer.set_current_language("ja-JP")
  let ja_error_message = ui_localizer.localize_error_message("error.required_field", [
    ("field_name", "ユーザー名")
  ])
  assert_eq(ja_error_message, "フィールド ユーザー名 は必須です")
}

test "文化特定的格式和规则" {
  // 创建文化规则处理器
  let culture_handler = azimuth::CultureHandler::new()
  
  // 配置不同文化的规则
  culture_handler.set_culture_rules("zh-CN", azimuth::CultureRules {
    name_format: azimuth::NameFormat::FamilyNameFirst, // 姓在前
    address_format: azimuth::AddressFormat::CountryCityStateStreet, // 国-城市-省-街道
    reading_direction: azimuth::ReadingDirection::LeftToRight,
    weekend_days: [6, 0], // 周六和周日
    first_day_of_week: 1, // 周一
    paper_size: azimuth::PaperSize::A4,
    measurement_system: azimuth::MeasurementSystem::Metric
  })
  
  culture_handler.set_culture_rules("en-US", azimuth::CultureRules {
    name_format: azimuth::NameFormat::GivenNameFirst, // 名在前
    address_format: azimuth::AddressFormat::StreetCityStateCountry, // 街道-城市-省-国家
    reading_direction: azimuth::ReadingDirection::LeftToRight,
    weekend_days: [6, 0], // 周六和周日
    first_day_of_week: 0, // 周日
    paper_size: azimuth::PaperSize::Letter,
    measurement_system: azimuth::MeasurementSystem::Imperial
  })
  
  culture_handler.set_culture_rules("ja-JP", azimuth::CultureRules {
    name_format: azimuth::NameFormat::FamilyNameFirst, // 姓在前
    address_format: azimuth::AddressFormat::PostalCodeCityStateStreet, // 邮编-城市-省-街道
    reading_direction: azimuth::ReadingDirection::LeftToRight, // 现代日语通常从左到右
    weekend_days: [6, 0], // 周六和周日
    first_day_of_week: 0, // 周日
    paper_size: azimuth::PaperSize::A4,
    measurement_system: azimuth::MeasurementSystem::Metric
  })
  
  // 测试姓名格式
  let person_name = azimuth::PersonName::new("张", "三")
  
  culture_handler.set_current_culture("zh-CN")
  let zh_formatted_name = culture_handler.format_name(person_name)
  assert_eq(zh_formatted_name, "张三") // 姓在前
  
  culture_handler.set_current_culture("en-US")
  let en_formatted_name = culture_handler.format_name(person_name)
  assert_eq(en_formatted_name, "三 张") // 名在前
  
  // 测试地址格式
  let address = azimuth::Address::new()
  address.street = "中关村大街1号"
  address.city = "北京"
  address.state = "北京市"
  address.postal_code = "100080"
  address.country = "中国"
  
  culture_handler.set_current_culture("zh-CN")
  let zh_formatted_address = culture_handler.format_address(address)
  assert_eq(zh_formatted_address, "中国 北京市 北京 中关村大街1号 100080")
  
  culture_handler.set_current_culture("en-US")
  let en_formatted_address = culture_handler.format_address(address)
  assert_eq(en_formatted_address, "中关村大街1号, 北京, 北京市, 中国 100080")
  
  // 测试工作日和周末判断
  culture_handler.set_current_culture("zh-CN")
  assert_false(culture_handler.is_weekend(1)) // 周一不是周末
  assert_false(culture_handler.is_weekend(5)) // 周五不是周末
  assert_true(culture_handler.is_weekend(6))  // 周六是周末
  assert_true(culture_handler.is_weekend(0))  // 周日是周末
  
  // 测试度量单位转换
  let length_in_meters = 1.80 // 1.8米
  
  culture_handler.set_current_culture("zh-CN")
  let zh_length = culture_handler.format_length(length_in_meters)
  assert_eq(zh_length, "1.80米") // 使用公制
  
  culture_handler.set_current_culture("en-US")
  let en_length = culture_handler.format_length(length_in_meters)
  assert_eq(en_length, "5'11\"") // 转换为英制英尺和英寸
  
  // 测试重量转换
  let weight_in_kg = 70.0 // 70公斤
  
  culture_handler.set_current_culture("zh-CN")
  let zh_weight = culture_handler.format_weight(weight_in_kg)
  assert_eq(zh_weight, "70.0公斤") // 使用公制
  
  culture_handler.set_current_culture("en-US")
  let en_weight = culture_handler.format_weight(weight_in_kg)
  assert_eq(en_weight, "154.3磅") // 转换为英制磅
}