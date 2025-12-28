// 国际化支持测试用例
// 测试遥测系统对不同语言、字符集和地区设置的支持

test "multilingual log messages and attributes" {
  // 测试多语言日志消息和属性
  let logger = azimuth::LoggerProvider::get_logger(
    azimuth::LoggerProvider::default(), 
    "internationalization-logger"
  )
  
  // 中文日志消息
  let chinese_log = azimuth::LogRecord::new(azimuth::Info, "用户登录成功，用户ID: 12345")
  assert_eq(azimuth::LogRecord::body(chinese_log), Some("用户登录成功，用户ID: 12345"))
  
  // 日文日志消息
  let japanese_log = azimuth::LogRecord::new(azimuth::Info, "ユーザーログイン成功、ユーザーID: 12345")
  assert_eq(azimuth::LogRecord::body(japanese_log), Some("ユーザーログイン成功、ユーザーID: 12345"))
  
  // 韩文日志消息
  let korean_log = azimuth::LogRecord::new(azimuth::Info, "사용자 로그인 성공, 사용자 ID: 12345")
  assert_eq(azimuth::LogRecord::body(korean_log), Some("사용자 로그인 성공, 사용자 ID: 12345"))
  
  // 阿拉伯文日志消息
  let arabic_log = azimuth::LogRecord::new(azimuth::Info, "تم تسجيل دخول المستخدم بنجاح، معرف المستخدم: 12345")
  assert_eq(azimuth::LogRecord::body(arabic_log), Some("تم تسجيل دخول المستخدم بنجاح، معرف المستخدم: 12345"))
  
  // 俄文日志消息
  let russian_log = azimuth::LogRecord::new(azimuth::Info, "Пользователь успешно вошел в систему, ID пользователя: 12345")
  assert_eq(azimuth::LogRecord::body(russian_log), Some("Пользователь успешно вошел в систему, ID пользователя: 12345"))
  
  // 法文日志消息
  let french_log = azimuth::LogRecord::new(azimuth::Info, "Utilisateur connecté avec succès, ID utilisateur: 12345")
  assert_eq(azimuth::LogRecord::body(french_log), Some("Utilisateur connecté avec succès, ID utilisateur: 12345"))
  
  // 德文日志消息
  let german_log = azimuth::LogRecord::new(azimuth::Info, "Benutzer erfolgreich angemeldet, Benutzer-ID: 12345")
  assert_eq(azimuth::LogRecord::body(german_log), Some("Benutzer erfolgreich angemeldet, Benutzer-ID: 12345"))
  
  // 西班牙文日志消息
  let spanish_log = azimuth::LogRecord::new(azimuth::Info, "Usuario inició sesión correctamente, ID de usuario: 12345")
  assert_eq(azimuth::LogRecord::body(spanish_log), Some("Usuario inició sesión correctamente, ID de usuario: 12345"))
  
  // 发射所有日志
  azimuth::Logger::emit(logger, chinese_log)
  azimuth::Logger::emit(logger, japanese_log)
  azimuth::Logger::emit(logger, korean_log)
  azimuth::Logger::emit(logger, arabic_log)
  azimuth::Logger::emit(logger, russian_log)
  azimuth::Logger::emit(logger, french_log)
  azimuth::Logger::emit(logger, german_log)
  azimuth::Logger::emit(logger, spanish_log)
}

test "multilingual span names and events" {
  // 测试多语言span名称和事件
  let tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "internationalization-tracer"
  )
  
  // 中文span
  let chinese_span = azimuth::Tracer::start_span(tracer, "用户认证流程")
  azimuth::Span::add_event(chinese_span, "开始验证用户凭据")
  azimuth::Span::add_event(chinese_span, "数据库查询完成")
  azimuth::Span::add_event(chinese_span, "用户认证成功")
  azimuth::Span::end(chinese_span)
  
  // 日文span
  let japanese_span = azimuth::Tracer::start_span(tracer, "ユーザー認証フロー")
  azimuth::Span::add_event(japanese_span, "ユーザー認証情報の検証を開始")
  azimuth::Span::add_event(japanese_span, "データベースクエリ完了")
  azimuth::Span::add_event(japanese_span, "ユーザー認証成功")
  azimuth::Span::end(japanese_span)
  
  // 韩文span
  let korean_span = azimuth::Tracer::start_span(tracer, "사용자 인증 흐름")
  azimuth::Span::add_event(korean_span, "사용자 자격 증명 확인 시작")
  azimuth::Span::add_event(korean_span, "데이터베이스 쿼리 완료")
  azimuth::Span::add_event(korean_span, "사용자 인증 성공")
  azimuth::Span::end(korean_span)
  
  // 阿拉伯文span
  let arabic_span = azimuth::Tracer::start_span(tracer, "مسار مصادقة المستخدم")
  azimuth::Span::add_event(arabic_span, "بدء التحقق من بيانات اعتماد المستخدم")
  azimuth::Span::add_event(arabic_span, "اكتمل استعلام قاعدة البيانات")
  azimuth::Span::add_event(arabic_span, "نجحت مصادقة المستخدم")
  azimuth::Span::end(arabic_span)
  
  // 验证span名称
  assert_eq(azimuth::Span::name(chinese_span), "用户认证流程")
  assert_eq(azimuth::Span::name(japanese_span), "ユーザー認証フロー")
  assert_eq(azimuth::Span::name(korean_span), "사용자 인증 흐름")
  assert_eq(azimuth::Span::name(arabic_span), "مسار مصادقة المستخدم")
}

test "unicode attribute values" {
  // 测试Unicode属性值
  let attrs = azimuth::Attributes::new()
  
  // 中文属性值
  azimuth::Attributes::set(attrs, "用户名", azimuth::StringValue("张三"))
  azimuth::Attributes::set(attrs, "城市", azimuth::StringValue("北京"))
  
  // 日文属性值
  azimuth::Attributes::set(attrs, "ユーザー名", azimuth::StringValue("田中太郎"))
  azimuth::Attributes::set(attrs, "都市", azimuth::StringValue("東京"))
  
  // 韩文属性值
  azimuth::Attributes::set(attrs, "사용자명", azimuth::StringValue("김철수"))
  azimuth::Attributes::set(attrs, "도시", azimuth::StringValue("서울"))
  
  // 阿拉伯文属性值
  azimuth::Attributes::set(attrs, "اسم المستخدم", azimuth::StringValue("أحمد محمد"))
  azimuth::Attributes::set(attrs, "المدينة", azimuth::StringValue("الرياض"))
  
  // 俄文属性值
  azimuth::Attributes::set(attrs, "имя пользователя", azimuth::StringValue("Иван Петров"))
  azimuth::Attributes::set(attrs, "город", azimuth::StringValue("Москва"))
  
  // Emoji属性值
  azimuth::Attributes::set(attrs, "status", azimuth::StringValue("✅ 成功"))
  azimuth::Attributes::set(attrs, "mood", azimuth::StringValue("😊 开心"))
  azimuth::Attributes::set(attrs, "weather", azimuth::StringValue("🌤️ 多云"))
  
  // 验证属性值
  assert_eq(azimuth::Attributes::get(attrs, "用户名"), Some(azimuth::StringValue("张三")))
  assert_eq(azimuth::Attributes::get(attrs, "ユーザー名"), Some(azimuth::StringValue("田中太郎")))
  assert_eq(azimuth::Attributes::get(attrs, "사용자명"), Some(azimuth::StringValue("김철수")))
  assert_eq(azimuth::Attributes::get(attrs, "اسم المستخدم"), Some(azimuth::StringValue("أحمد محمد")))
  assert_eq(azimuth::Attributes::get(attrs, "имя пользователя"), Some(azimuth::StringValue("Иван Петров")))
  assert_eq(azimuth::Attributes::get(attrs, "status"), Some(azimuth::StringValue("✅ 成功")))
}

test "locale-specific formatting" {
  // 测试特定地区的格式化
  
  // 日期时间格式化（不同地区）
  let chinese_date = "2025年12月28日 14:30:00"
  let japanese_date = "2025年12月28日 14:30:00"
  let korean_date = "2025년 12월 28일 14:30:00"
  let arabic_date = "٢٨ ديسمبر ٢٠٢٥ ١٤:٣٠:٠٠"
  let us_date = "12/28/2025 2:30:00 PM"
  let european_date = "28.12.2025 14:30:00"
  
  // 数字格式化（不同地区）
  let chinese_number = "1,234.56"
  let european_number = "1.234,56"
  let arabic_number = "١٬٢٣٤٫٥٦"
  
  // 货币格式化（不同地区）
  let chinese_currency = "¥1,234.56"
  let japanese_currency = "¥1,235"
  let korean_currency = "₩1,235"
  let us_currency = "$1,234.56"
  let european_currency = "€1.234,56"
  let arabic_currency = "١٬٢٣٤٫٥٦ ر.س"
  
  // 将这些格式化信息存储为属性
  let attrs = azimuth::Attributes::new()
  
  // 日期时间
  azimuth::Attributes::set(attrs, "date.zh_CN", azimuth::StringValue(chinese_date))
  azimuth::Attributes::set(attrs, "date.ja_JP", azimuth::StringValue(japanese_date))
  azimuth::Attributes::set(attrs, "date.ko_KR", azimuth::StringValue(korean_date))
  azimuth::Attributes::set(attrs, "date.ar_SA", azimuth::StringValue(arabic_date))
  azimuth::Attributes::set(attrs, "date.en_US", azimuth::StringValue(us_date))
  azimuth::Attributes::set(attrs, "date.de_DE", azimuth::StringValue(european_date))
  
  // 数字
  azimuth::Attributes::set(attrs, "number.zh_CN", azimuth::StringValue(chinese_number))
  azimuth::Attributes::set(attrs, "number.de_DE", azimuth::StringValue(european_number))
  azimuth::Attributes::set(attrs, "number.ar_SA", azimuth::StringValue(arabic_number))
  
  // 货币
  azimuth::Attributes::set(attrs, "currency.zh_CN", azimuth::StringValue(chinese_currency))
  azimuth::Attributes::set(attrs, "currency.ja_JP", azimuth::StringValue(japanese_currency))
  azimuth::Attributes::set(attrs, "currency.ko_KR", azimuth::StringValue(korean_currency))
  azimuth::Attributes::set(attrs, "currency.en_US", azimuth::StringValue(us_currency))
  azimuth::Attributes::set(attrs, "currency.de_DE", azimuth::StringValue(european_currency))
  azimuth::Attributes::set(attrs, "currency.ar_SA", azimuth::StringValue(arabic_currency))
  
  // 验证格式化信息
  assert_eq(azimuth::Attributes::get(attrs, "date.zh_CN"), Some(azimuth::StringValue(chinese_date)))
  assert_eq(azimuth::Attributes::get(attrs, "number.de_DE"), Some(azimuth::StringValue(european_number)))
  assert_eq(azimuth::get(attrs, "currency.ar_SA"), Some(azimuth::StringValue(arabic_currency)))
}

test "right-to-left language support" {
  // 测试从右到左的语言支持
  let attrs = azimuth::Attributes::new()
  
  // 阿拉伯文（从右到左）
  azimuth::Attributes::set(attrs, "الاسم", azimuth::StringValue("أحمد محمد"))
  azimuth::Attributes::set(attrs, "المدينة", azimuth::StringValue("الرياض"))
  azimuth::Attributes::set(attrs, "البلد", azimuth::StringValue("المملكة العربية السعودية"))
  azimuth::Attributes::set(attrs, "العملية", azimuth::StringValue("تسجيل الدخول"))
  
  // 希伯来文（从右到左）
  azimuth::Attributes::set(attrs, "שם", azimuth::StringValue("דוד כהן"))
  azimuth::Attributes::set(attrs, "עיר", azimuth::StringValue("ירושלים"))
  azimuth::Attributes::set(attrs, "מדינה", azimuth::StringValue("ישראל"))
  azimuth::Attributes::set(attrs, "פעולה", azimuth::StringValue("כניסה למערכת"))
  
  // 波斯文（从右到左）
  azimuth::Attributes::set(attrs, "نام", azimuth::StringValue("علی رضایی"))
  azimuth::Attributes::set(attrs, "شهر", azimuth::StringValue("تهران"))
  azimuth::Attributes::set(attrs, "کشور", azimuth::StringValue("ایران"))
  azimuth::Attributes::set(attrs, "عملیات", azimuth::StringValue("ورود به سیستم"))
  
  // 乌尔都文（从右到左）
  azimuth::Attributes::set(attrs, "نام", azimuth::StringValue("محمد علی"))
  azimuth::Attributes::set(attrs, "شہر", azimuth::StringValue("کراچی"))
  azimuth::Attributes::set(attrs, "ملک", azimuth::StringValue("پاکستان"))
  azimuth::Attributes::set(attrs, "عمل", azimuth::StringValue("سسٹم میں لاگ ان"))
  
  // 验证从右到左的文本
  assert_eq(azimuth::Attributes::get(attrs, "الاسم"), Some(azimuth::StringValue("أحمد محمد")))
  assert_eq(azimuth::Attributes::get(attrs, "שם"), Some(azimuth::StringValue("דוד כהן")))
  assert_eq(azimuth::Attributes::get(attrs, "نام"), Some(azimuth::StringValue("علی رضایی")))
  
  // 创建包含从右到左文本的日志记录
  let logger = azimuth::LoggerProvider::get_logger(
    azimuth::LoggerProvider::default(), 
    "rtl-logger"
  )
  
  let arabic_log = azimuth::LogRecord::new(azimuth::Info, "تم تسجيل دخول المستخدم بنجاح")
  let hebrew_log = azimuth::LogRecord::new(azimuth::Info, "המשתמש נכנס למערכת בהצלחה")
  let persian_log = azimuth::LogRecord::new(azimuth::Info, "کاربر با موفقیت وارد سیستم شد")
  let urdu_log = azimuth::LogRecord::new(azimuth::Info, "صارف کامیابی سے سسٹم میں داخل ہوا")
  
  // 发射日志
  azimuth::Logger::emit(logger, arabic_log)
  azimuth::Logger::emit(logger, hebrew_log)
  azimuth::Logger::emit(logger, persian_log)
  azimuth::Logger::emit(logger, urdu_log)
  
  // 验证日志内容
  assert_eq(azimuth::LogRecord::body(arabic_log), Some("تم تسجيل دخول المستخدم بنجاح"))
  assert_eq(azimuth::LogRecord::body(hebrew_log), Some("המשתמש נכנס למערכת בהצלחה"))
  assert_eq(azimuth::LogRecord::body(persian_log), Some("کاربر با موفقیت وارد سیستم شد"))
  assert_eq(azimuth::LogRecord::body(urdu_log), Some("صارف کامیابی سے سسٹم میں داخل ہوا"))
}

test "mixed language content" {
  // 测试混合语言内容
  let attrs = azimuth::Attributes::new()
  
  // 混合语言的错误消息
  azimuth::Attributes::set(attrs, "error.message", azimuth::StringValue("用户登录失败: Invalid credentials"))
  azimuth::Attributes::set(attrs, "error.description", azimuth::StringValue("ユーザーログイン失敗: パスワードが正しくありません"))
  azimuth::Attributes::set(attrs, "error.resolution", azimuth::StringValue("사용자 로그인 실패: 비밀번호를 재확인하세요"))
  
  // 混合语言的span名称
  let tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "mixed-language-tracer"
  )
  
  let mixed_span = azimuth::Tracer::start_span(tracer, "User Authentication | ユーザー認証 | 사용자 인증")
  azimuth::Span::add_event(mixed_span, "开始验证用户凭据 | ユーザー認証情報の検証を開始 | 사용자 자격 증명 확인 시작")
  azimuth::Span::add_event(mixed_span, "数据库查询完成 | データベースクエリ完了 | 데이터베이스 쿼리 완료")
  azimuth::Span::add_event(mixed_span, "用户认证成功 | ユーザー認証成功 | 사용자 인증 성공")
  azimuth::Span::end(mixed_span)
  
  // 混合语言的日志记录
  let logger = azimuth::LoggerProvider::get_logger(
    azimuth::LoggerProvider::default(), 
    "mixed-language-logger"
  )
  
  let mixed_log = azimuth::LogRecord::new(azimuth::Info, "用户张三成功登录系统 | ユーザー田中太郎がシステムに正常にログインしました | 사용자 김철수가 시스템에 성공적으로 로그인했습니다")
  
  // 发射日志
  azimuth::Logger::emit(logger, mixed_log)
  
  // 验证混合语言内容
  assert_eq(azimuth::Span::name(mixed_span), "User Authentication | ユーザー認証 | 사용자 인증")
  assert_eq(azimuth::LogRecord::body(mixed_log), Some("用户张三成功登录系统 | ユーザー田中太郎がシステムに正常にログインしました | 사용자 김철수가 시스템에 성공적으로 로그인했습니다"))
  
  // 验证混合语言属性
  assert_eq(azimuth::Attributes::get(attrs, "error.message"), Some(azimuth::StringValue("用户登录失败: Invalid credentials")))
  assert_eq(azimuth::Attributes::get(attrs, "error.description"), Some(azimuth::StringValue("ユーザーログイン失敗: パスワードが正しくありません")))
  assert_eq(azimuth::Attributes::get(attrs, "error.resolution"), Some(azimuth::StringValue("사용자 로그인 실패: 비밀번호를 재확인하세요")))
}