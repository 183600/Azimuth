// Azimuth Telemetry System - Comprehensive Internationalization Tests
// 综合国际化测试用例，测试系统的多语言和本地化支持

test "多语言文本处理测试" {
  // 测试中文文本处理
  let chinese_text = "遥测系统监控数据"
  assert_eq(chinese_text.length(), 8)
  assert_eq(chinese_text.char_at(0), "遥")
  assert_eq(chinese_text.char_at(7), "据")
  
  // 测试日文文本处理
  let japanese_text = "テレメトリシステム"
  assert_eq(japanese_text.length(), 8)
  assert_eq(japanese_text.char_at(0), "テ")
  assert_eq(japanese_text.char_at(7), "ム")
  
  // 测试韩文文本处理
  let korean_text = "원격 측정 시스템"
  assert_eq(korean_text.length(), 8)
  assert_eq(korean_text.char_at(0), "원")
  assert_eq(korean_text.char_at(7), "템")
  
  // 测试阿拉伯文文本处理
  let arabic_text = "نظام القياس عن بعد"
  assert_eq(arabic_text.length(), 16)
  
  // 测试俄文文本处理
  let russian_text = "Система телеметрии"
  assert_eq(russian_text.length(), 17)
  
  // 测试混合语言文本处理
  let mixed_text = "Azimuth遥测系统テレメトリー"
  assert_eq(mixed_text.length(), 16)
  
  // 测试Unicode编码转换
  let unicode_text = "🌍📊📈📉"
  assert_eq(unicode_text.length(), 4)
  assert_eq(unicode_text.code_point_at(0), 127757) // 🌍
  assert_eq(unicode_text.code_point_at(1), 128202) // 📊
  assert_eq(unicode_text.code_point_at(2), 128200) // 📈
  assert_eq(unicode_text.code_point_at(3), 128201) // 📉
}

test "本地化数字和日期格式测试" {
  // 测试数字格式化
  let number = 1234567.89
  
  // 美国格式
  let us_format = azimuth::Locale::format_number(number, "en-US")
  assert_eq(us_format, "1,234,567.89")
  
  // 德国格式
  let de_format = azimuth::Locale::format_number(number, "de-DE")
  assert_eq(de_format, "1.234.567,89")
  
  // 法国格式
  let fr_format = azimuth::Locale::format_number(number, "fr-FR")
  assert_eq(fr_format, "1 234 567,89")
  
  // 中国格式
  let zh_format = azimuth::Locale::format_number(number, "zh-CN")
  assert_eq(zh_format, "1,234,567.89")
  
  // 印度格式
  let hi_format = azimuth::Locale::format_number(number, "hi-IN")
  assert_eq(hi_format, "12,34,567.89")
  
  // 测试日期格式化
  let date = azimuth::DateTime::new(2024, 1, 15, 14, 30, 0)
  
  // 美国格式
  let us_date = azimuth::Locale::format_date(date, "en-US")
  assert_eq(us_date, "1/15/2024, 2:30 PM")
  
  // 英国格式
  let uk_date = azimuth::Locale::format_date(date, "en-GB")
  assert_eq(uk_date, "15/01/2024, 14:30")
  
  // 中国格式
  let zh_date = azimuth::Locale::format_date(date, "zh-CN")
  assert_eq(zh_date, "2024/1/15 下午2:30")
  
  // 日本格式
  let ja_date = azimuth::Locale::format_date(date, "ja-JP")
  assert_eq(ja_date, "2024/1/15 14:30")
  
  // 德国格式
  let de_date = azimuth::Locale::format_date(date, "de-DE")
  assert_eq(de_date, "15.1.2024, 14:30")
}

test "多语言错误消息测试" {
  // 创建多语言错误处理器
  let error_handler = azimuth::I18nErrorHandler::new()
  
  // 添加错误消息翻译
  error_handler.add_translation("en", "connection_failed", "Connection to server failed")
  error_handler.add_translation("zh", "connection_failed", "连接服务器失败")
  error_handler.add_translation("ja", "connection_failed", "サーバーへの接続に失敗しました")
  error_handler.add_translation("ko", "connection_failed", "서버 연결에 실패했습니다")
  error_handler.add_translation("fr", "connection_failed", "La connexion au serveur a échoué")
  
  error_handler.add_translation("en", "invalid_credentials", "Invalid username or password")
  error_handler.add_translation("zh", "invalid_credentials", "用户名或密码无效")
  error_handler.add_translation("ja", "invalid_credentials", "ユーザー名またはパスワードが無効です")
  error_handler.add_translation("ko", "invalid_credentials", "사용자 이름 또는 비밀번호가 잘못되었습니다")
  error_handler.add_translation("fr", "invalid_credentials", "Nom d'utilisateur ou mot de passe invalide")
  
  // 测试英语错误消息
  let en_error1 = error_handler.get_message("en", "connection_failed")
  assert_eq(en_error1, "Connection to server failed")
  
  let en_error2 = error_handler.get_message("en", "invalid_credentials")
  assert_eq(en_error2, "Invalid username or password")
  
  // 测试中文错误消息
  let zh_error1 = error_handler.get_message("zh", "connection_failed")
  assert_eq(zh_error1, "连接服务器失败")
  
  let zh_error2 = error_handler.get_message("zh", "invalid_credentials")
  assert_eq(zh_error2, "用户名或密码无效")
  
  // 测试日文错误消息
  let ja_error1 = error_handler.get_message("ja", "connection_failed")
  assert_eq(ja_error1, "サーバーへの接続に失敗しました")
  
  let ja_error2 = error_handler.get_message("ja", "invalid_credentials")
  assert_eq(ja_error2, "ユーザー名またはパスワードが無効です")
  
  // 测试韩文错误消息
  let ko_error1 = error_handler.get_message("ko", "connection_failed")
  assert_eq(ko_error1, "서버 연결에 실패했습니다")
  
  let ko_error2 = error_handler.get_message("ko", "invalid_credentials")
  assert_eq(ko_error2, "사용자 이름 또는 비밀번호가 잘못되었습니다")
  
  // 测试法文错误消息
  let fr_error1 = error_handler.get_message("fr", "connection_failed")
  assert_eq(fr_error1, "La connexion au serveur a échoué")
  
  let fr_error2 = error_handler.get_message("fr", "invalid_credentials")
  assert_eq(fr_error2, "Nom d'utilisateur ou mot de passe invalide")
  
  // 测试回退到默认语言
  let fallback_error = error_handler.get_message("es", "connection_failed") // 西班牙语未定义
  assert_eq(fallback_error, "Connection to server failed") // 回退到英语
}

test "文化特定排序测试" {
  // 测试不同语言的字符串排序
  let strings = ["apple", "banana", "cherry", "date"]
  
  // 英语排序
  let en_sorted = azimuth::Locale::sort_strings(strings, "en-US")
  assert_eq(en_sorted, ["apple", "banana", "cherry", "date"])
  
  // 测试中文排序
  let chinese_strings = ["苹果", "香蕉", "樱桃", "枣"]
  let zh_sorted = azimuth::Locale::sort_strings(chinese_strings, "zh-CN")
  assert_eq(zh_sorted, ["枣", "苹果", "樱桃", "香蕉"])
  
  // 测试日文排序
  let japanese_strings = ["りんご", "ばなな", "さくらんぼ", "なつめ"]
  let ja_sorted = azimuth::Locale::sort_strings(japanese_strings, "ja-JP")
  assert_eq(ja_sorted, ["ばなな", "さくらんぼ", "なつめ", "りんご"])
  
  // 测试德语特殊字符排序
  let german_strings = ["äpfel", "bananen", "kirschen", "datteln"]
  let de_sorted = azimuth::Locale::sort_strings(german_strings, "de-DE")
  assert_eq(de_sorted, ["äpfel", "bananen", "datteln", "kirschen"])
  
  // 测试西班牙语特殊字符排序
  let spanish_strings = ["manzana", "banana", "cereza", "dátil"]
  let es_sorted = azimuth::Locale::sort_strings(spanish_strings, "es-ES")
  assert_eq(es_sorted, ["banana", "cereza", "dátil", "manzana"])
}

test "时区和时间转换测试" {
  // 创建UTC时间
  let utc_time = azimuth::DateTime::from_utc(2024, 1, 15, 12, 0, 0)
  
  // 转换为不同时区
  let us_eastern = utc_time.to_timezone("America/New_York")
  assert_eq(us_eastern.hour(), 7) // UTC-5 (标准时间)
  assert_eq(us_eastern.minute(), 0)
  
  let us_pacific = utc_time.to_timezone("America/Los_Angeles")
  assert_eq(us_pacific.hour(), 4) // UTC-8 (标准时间)
  assert_eq(us_pacific.minute(), 0)
  
  let europe_london = utc_time.to_timezone("Europe/London")
  assert_eq(europe_london.hour(), 12) // UTC+0 (标准时间)
  assert_eq(europe_london.minute(), 0)
  
  let europe_paris = utc_time.to_timezone("Europe/Paris")
  assert_eq(europe_paris.hour(), 13) // UTC+1 (标准时间)
  assert_eq(europe_paris.minute(), 0)
  
  let asia_tokyo = utc_time.to_timezone("Asia/Tokyo")
  assert_eq(asia_tokyo.hour(), 21) // UTC+9
  assert_eq(asia_tokyo.minute(), 0)
  
  let asia_shanghai = utc_time.to_timezone("Asia/Shanghai")
  assert_eq(asia_shanghai.hour(), 20) // UTC+8
  assert_eq(asia_shanghai.minute(), 0)
  
  let australia_sydney = utc_time.to_timezone("Australia/Sydney")
  assert_eq(australia_sydney.hour(), 23) // UTC+11
  assert_eq(australia_sydney.minute(), 0)
  
  // 测试夏令时转换
  let summer_utc = azimuth::DateTime::from_utc(2024, 7, 15, 12, 0, 0)
  let summer_eastern = summer_utc.to_timezone("America/New_York")
  assert_eq(summer_eastern.hour(), 8) // UTC-4 (夏令时)
  
  // 测试时区格式化
  let formatted_utc = utc_time.format_with_timezone("en-US", "UTC")
  assert_eq(formatted_utc, "1/15/2024, 12:00 PM UTC")
  
  let formatted_eastern = us_eastern.format_with_timezone("en-US", "EST")
  assert_eq(formatted_eastern, "1/15/2024, 7:00 AM EST")
  
  let formatted_tokyo = asia_tokyo.format_with_timezone("ja-JP", "JST")
  assert_eq(formatted_tokyo, "2024/1/15 21:00 JST")
}

test "货币和度量衡本地化测试" {
  // 测试货币格式化
  let amount = 1234.56
  
  // 美元
  let usd = azimuth::Locale::format_currency(amount, "en-US", "USD")
  assert_eq(usd, "$1,234.56")
  
  // 欧元
  let eur = azimuth::Locale::format_currency(amount, "de-DE", "EUR")
  assert_eq(eur, "1.234,56 €")
  
  // 人民币
  let cny = azimuth::Locale::format_currency(amount, "zh-CN", "CNY")
  assert_eq(cny, "¥1,234.56")
  
  // 日元
  let jpy = azimuth::Locale::format_currency(amount, "ja-JP", "JPY")
  assert_eq(jpy, "¥1,235") // 日元通常不显示小数
  
  // 英镑
  let gbp = azimuth::Locale::format_currency(amount, "en-GB", "GBP")
  assert_eq(gbp, "£1,234.56")
  
  // 韩元
  let krw = azimuth::Locale::format_currency(amount, "ko-KR", "KRW")
  assert_eq(krw, "₩1,235") // 韩元通常不显示小数
  
  // 测试度量衡单位转换
  let distance = 100.0 // 英里
  
  // 转换为公里（公制）
  let km = azimuth::Locale::convert_distance(distance, "mi", "km")
  assert_true((km - 160.934).abs() < 0.001)
  
  // 转换为公里（美国本地化）
  let us_km = azimuth::Locale::format_distance(distance, "mi", "km", "en-US")
  assert_eq(us_km, "160.934 km")
  
  // 转换为英里（美国本地化）
  let us_mi = azimuth::Locale::format_distance(km, "km", "mi", "en-US")
  assert_eq(us_mi, "100.0 mi")
  
  // 转换为公里（中国本地化）
  let cn_km = azimuth::Locale::format_distance(distance, "mi", "km", "zh-CN")
  assert_eq(cn_km, "160.934公里")
  
  // 测试重量单位转换
  let weight = 100.0 // 磅
  
  // 转换为公斤
  let kg = azimuth::Locale::convert_weight(weight, "lb", "kg")
  assert_true((kg - 45.359).abs() < 0.001)
  
  // 格式化重量（美国本地化）
  let us_kg = azimuth::Locale::format_weight(weight, "lb", "kg", "en-US")
  assert_eq(us_kg, "45.359 kg")
  
  // 格式化重量（中国本地化）
  let cn_kg = azimuth::Locale::format_weight(weight, "lb", "kg", "zh-CN")
  assert_eq(cn_kg, "45.359公斤")
}

test "多语言UI元素测试" {
  // 创建多语言UI管理器
  let ui_manager = azimuth::I18nUIManager::new()
  
  // 添加UI元素翻译
  ui_manager.add_translation("en", "button_submit", "Submit")
  ui_manager.add_translation("zh", "button_submit", "提交")
  ui_manager.add_translation("ja", "button_submit", "送信")
  ui_manager.add_translation("ko", "button_submit", "제출")
  ui_manager.add_translation("fr", "button_submit", "Soumettre")
  
  ui_manager.add_translation("en", "button_cancel", "Cancel")
  ui_manager.add_translation("zh", "button_cancel", "取消")
  ui_manager.add_translation("ja", "button_cancel", "キャンセル")
  ui_manager.add_translation("ko", "button_cancel", "취소")
  ui_manager.add_translation("fr", "button_cancel", "Annuler")
  
  ui_manager.add_translation("en", "menu_dashboard", "Dashboard")
  ui_manager.add_translation("zh", "menu_dashboard", "仪表板")
  ui_manager.add_translation("ja", "menu_dashboard", "ダッシュボード")
  ui_manager.add_translation("ko", "menu_dashboard", "대시보드")
  ui_manager.add_translation("fr", "menu_dashboard", "Tableau de bord")
  
  // 测试英语UI元素
  let en_submit = ui_manager.get_text("en", "button_submit")
  assert_eq(en_submit, "Submit")
  
  let en_cancel = ui_manager.get_text("en", "button_cancel")
  assert_eq(en_cancel, "Cancel")
  
  let en_dashboard = ui_manager.get_text("en", "menu_dashboard")
  assert_eq(en_dashboard, "Dashboard")
  
  // 测试中文UI元素
  let zh_submit = ui_manager.get_text("zh", "button_submit")
  assert_eq(zh_submit, "提交")
  
  let zh_cancel = ui_manager.get_text("zh", "button_cancel")
  assert_eq(zh_cancel, "取消")
  
  let zh_dashboard = ui_manager.get_text("zh", "menu_dashboard")
  assert_eq(zh_dashboard, "仪表板")
  
  // 测试日文UI元素
  let ja_submit = ui_manager.get_text("ja", "button_submit")
  assert_eq(ja_submit, "送信")
  
  let ja_cancel = ui_manager.get_text("ja", "button_cancel")
  assert_eq(ja_cancel, "キャンセル")
  
  let ja_dashboard = ui_manager.get_text("ja", "menu_dashboard")
  assert_eq(ja_dashboard, "ダッシュボード")
  
  // 测试韩文UI元素
  let ko_submit = ui_manager.get_text("ko", "button_submit")
  assert_eq(ko_submit, "제출")
  
  let ko_cancel = ui_manager.get_text("ko", "button_cancel")
  assert_eq(ko_cancel, "취소")
  
  let ko_dashboard = ui_manager.get_text("ko", "menu_dashboard")
  assert_eq(ko_dashboard, "대시보드")
  
  // 测试法文UI元素
  let fr_submit = ui_manager.get_text("fr", "button_submit")
  assert_eq(fr_submit, "Soumettre")
  
  let fr_cancel = ui_manager.get_text("fr", "button_cancel")
  assert_eq(fr_cancel, "Annuler")
  
  let fr_dashboard = ui_manager.get_text("fr", "menu_dashboard")
  assert_eq(fr_dashboard, "Tableau de bord")
  
  // 测试参数化文本
  ui_manager.add_translation("en", "welcome_message", "Welcome, {name}!")
  ui_manager.add_translation("zh", "welcome_message", "欢迎，{name}！")
  ui_manager.add_translation("ja", "welcome_message", "ようこそ、{name}さん！")
  
  let en_welcome = ui_manager.get_formatted_text("en", "welcome_message", [("name", "张三")])
  assert_eq(en_welcome, "Welcome, 张三!")
  
  let zh_welcome = ui_manager.get_formatted_text("zh", "welcome_message", [("name", "张三")])
  assert_eq(zh_welcome, "欢迎，张三！")
  
  let ja_welcome = ui_manager.get_formatted_text("ja", "welcome_message", [("name", "张三")])
  assert_eq(ja_welcome, "ようこそ、张三さん！")
}

test "文本方向和布局测试" {
  // 测试从左到右的文本方向
  let ltr_text = "Hello World"
  assert_eq(azimuth::Locale::get_text_direction("en"), "ltr")
  assert_eq(azimuth::Locale::get_text_direction("fr"), "ltr")
  assert_eq(azimuth::Locale::get_text_direction("de"), "ltr")
  
  // 测试从右到左的文本方向
  assert_eq(azimuth::Locale::get_text_direction("ar"), "rtl")
  assert_eq(azimuth::Locale::get_text_direction("he"), "rtl")
  assert_eq(azimuth::Locale::get_text_direction("fa"), "rtl")
  
  // 测试阿拉伯语文本
  let arabic_text = "مرحبا بالعالم"
  let arabic_formatted = azimuth::Locale::format_text_with_direction(arabic_text, "ar")
  assert_eq(arabic_formatted, "مرحبا بالعالم") // 文本应该保持原样，但布局应该从右到左
  
  // 测试希伯来语文本
  let hebrew_text = "שלום עולם"
  let hebrew_formatted = azimuth::Locale::format_text_with_direction(hebrew_text, "he")
  assert_eq(hebrew_formatted, "שלום עולם")
  
  // 测试混合方向文本
  let mixed_text = "Hello مرحبا World"
  let mixed_formatted = azimuth::Locale::format_text_with_direction(mixed_text, "en")
  assert_eq(mixed_formatted, "Hello مرحبا World")
  
  // 测试布局镜像
  let layout_elements = ["button", "text", "icon"]
  let ltr_layout = azimuth::Locale::mirror_layout_if_needed(layout_elements, "en")
  assert_eq(ltr_layout, ["button", "text", "icon"]) // 不需要镜像
  
  let rtl_layout = azimuth::Locale::mirror_layout_if_needed(layout_elements, "ar")
  assert_eq(rtl_layout, ["icon", "text", "button"]) // 需要镜像
}

test "文化特定数据处理测试" {
  // 测试姓名格式化
  let person = {
    "first_name": "张",
    "last_name": "三",
    "title": "先生"
  }
  
  // 西方格式
  let western_name = azimuth::Locale::format_name(person, "en-US")
  assert_eq(western_name, "Mr. San Zhang")
  
  // 中文格式
  let chinese_name = azimuth::Locale::format_name(person, "zh-CN")
  assert_eq(chinese_name, "张三先生")
  
  // 日文格式
  let japanese_name = azimuth::Locale::format_name(person, "ja-JP")
  assert_eq(japanese_name, "張三様")
  
  // 韩文格式
  let korean_name = azimuth::Locale::format_name(person, "ko-KR")
  assert_eq(korean_name, "장 삼 씨")
  
  // 测试地址格式化
  let address = {
    "street": "123 Main St",
    "city": "Beijing",
    "state": "Beijing",
    "postal_code": "100000",
    "country": "China"
  }
  
  // 美国格式
  let us_address = azimuth::Locale::format_address(address, "en-US")
  assert_eq(us_address, "123 Main St\nBeijing, 100000\nChina")
  
  // 中文格式
  let cn_address = azimuth::Locale::format_address(address, "zh-CN")
  assert_eq(cn_address, "中国北京市\n邮编：100000\nMain St 123号")
  
  // 测试电话号码格式化
  let phone_number = "+8613812345678"
  
  // 美国格式
  let us_phone = azimuth::Locale::format_phone_number(phone_number, "en-US")
  assert_eq(us_phone, "+86 138-1234-5678")
  
  // 中文格式
  let cn_phone = azimuth::Locale::format_phone_number(phone_number, "zh-CN")
  assert_eq(cn_phone, "+86 138 1234 5678")
  
  // 测试文化特定列表格式化
  let list_items = ["苹果", "香蕉", "樱桃"]
  
  // 英语列表格式
  let en_list = azimuth::Locale::format_list(list_items, "en-US")
  assert_eq(en_list, "apples, bananas, and cherries")
  
  // 中文列表格式
  let zh_list = azimuth::Locale::format_list(list_items, "zh-CN")
  assert_eq(zh_list, "苹果、香蕉和樱桃")
  
  // 日文列表格式
  let ja_list = azimuth::Locale::format_list(list_items, "ja-JP")
  assert_eq(ja_list, "リンゴ、バナナ、チェリー")
}