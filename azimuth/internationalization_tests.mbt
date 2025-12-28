// Internationalization and Globalization Test Suite for Azimuth
// This file contains comprehensive tests for internationalization features

// Test 1: Multi-language error messages and logging
pub test "multi-language error messages and logging" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "i18n-test")
  
  // Test error messages in different languages
  let error_messages = [
    ("en", "User authentication failed"),
    ("zh", "用户身份验证失败"),
    ("es", "Error de autenticación de usuario"),
    ("fr", "Échec de l'authentification de l'utilisateur"),
    ("de", "Benutzer-Authentifizierung fehlgeschlagen"),
    ("ja", "ユーザー認証に失敗しました"),
    ("ko", "사용자 인증 실패"),
    ("ru", "Ошибка аутентификации пользователя"),
    ("ar", "فشل مصادقة المستخدم"),
    ("hi", "उपयोगकर्ता प्रमाणीकरण विफल")
  ]
  
  // Create log records in different languages
  let log_records = []
  for (lang, message) in error_messages {
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Error,
      Some(message),
      None,
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      None,
      None,
      None
    )
    log_records.push((lang, log_record))
  }
  
  // Verify all log records were created with correct messages
  for (expected_lang, expected_message) in error_messages {
    let found = false
    for (actual_lang, record) in log_records {
      if actual_lang == expected_lang {
        assert_eq(azimuth::LogRecord::body(record), Some(expected_message))
        assert_eq(azimuth::LogRecord::severity_number(record), azimuth::Error)
        found = true
        break
      }
    }
    assert_true(found, "Log record for language " + expected_lang + " not found")
  }
}

// Test 2: Unicode and multi-byte character handling in attributes
pub test "unicode and multi-byte character handling in attributes" {
  let attrs = azimuth::Attributes::new()
  
  // Test with various Unicode characters and scripts
  let unicode_test_values = [
    // Chinese characters
    ("chinese.name", azimuth::StringValue("张三")),
    ("chinese.address", azimuth::StringValue("北京市朝阳区建国门外大街1号")),
    
    // Japanese characters
    ("japanese.name", azimuth::StringValue("田中太郎")),
    ("japanese.address", azimuth::StringValue("東京都千代田区丸の内1-2-3")),
    
    // Korean characters
    ("korean.name", azimuth::StringValue("김철수")),
    ("korean.address", azimuth::StringValue("서울특별시 강남구 테헤란로 123")),
    
    // Arabic characters
    ("arabic.name", azimuth::StringValue("أحمد محمد")),
    ("arabic.text", azimuth::StringValue("مرحبا بالعالم")),
    
    // Hebrew characters
    ("hebrew.name", azimuth::StringValue("דוד כהן")),
    ("hebrew.text", azimuth::StringValue("שלום עולם")),
    
    // Cyrillic characters (Russian)
    ("russian.name", azimuth::StringValue("Иван Петров")),
    ("russian.text", azimuth::StringValue("Привет мир")),
    
    // Hindi characters
    ("hindi.name", azimuth::StringValue("राम कुमार")),
    ("hindi.text", azimuth::StringValue("नमस्ते दुनिया")),
    
    // Thai characters
    ("thai.name", azimuth::StringValue("สมชาย ใจดี")),
    ("thai.text", azimuth::StringValue("สวัสดีชาวโลก")),
    
    // Emoji and symbols
    ("emoji.text", azimuth::StringValue("🚀🔥💯✨🎉🌟")),
    ("symbols.math", azimuth::StringValue("∑∏∫∆∇∂√∞≈≠≤≥±×÷")),
    ("symbols.currency", azimuth::StringValue("$€£¥₹₽₩₪₫₡₨₦₱₲₴₸₼₽")),
    
    // Mixed scripts
    ("mixed.text", azimuth::StringValue("Hello 你好 안녕하세요 مرحبا שלום Привет")),
    ("complex.mixed", azimuth::StringValue("User: 张三, ID: 12345, Status: ✅ Active"))
  ]
  
  // Set all Unicode attributes
  for (key, value) in unicode_test_values {
    azimuth::Attributes::set(attrs, key, value)
  }
  
  // Verify all Unicode attributes were stored correctly
  for (key, expected_value) in unicode_test_values {
    let actual_value = azimuth::Attributes::get(attrs, key)
    assert_eq(actual_value, Some(expected_value))
  }
}

// Test 3: Locale-specific formatting in telemetry data
pub test "locale-specific formatting in telemetry data" {
  let attrs = azimuth::Attributes::new()
  
  // Test locale-specific number formatting
  let locale_numbers = [
    ("en_US.decimal", azimuth::StringValue("1,234.56")),
    ("de_DE.decimal", azimuth::StringValue("1.234,56")),
    ("fr_FR.decimal", azimuth::StringValue("1 234,56")),
    ("zh_CN.decimal", azimuth::StringValue("1,234.56")),
    ("ja_JP.decimal", azimuth::StringValue("1,234.56")),
    ("ru_RU.decimal", azimuth::StringValue("1 234,56")),
    ("ar_SA.decimal", azimuth::StringValue("١٬٢٣٤٫٥٦")),
    ("hi_IN.decimal", azimuth::StringValue("1,234.56"))
  ]
  
  // Test locale-specific date/time formatting
  let locale_dates = [
    ("en_US.date", azimuth::StringValue("01/15/2025 03:30:45 PM")),
    ("de_DE.date", azimuth::StringValue("15.01.2025 15:30:45")),
    ("fr_FR.date", azimuth::StringValue("15/01/2025 15:30:45")),
    ("zh_CN.date", azimuth::StringValue("2025/01/15 15:30:45")),
    ("ja_JP.date", azimuth::StringValue("2025/01/15 15:30:45")),
    ("ru_RU.date", azimuth::StringValue("15.01.2025 15:30:45")),
    ("ar_SA.date", azimuth::StringValue("١٥/٠١/٢٠٢٥ ٠٣:٣٠:٤٥ م")),
    ("ko_KR.date", azimuth::StringValue("2025. 01. 15. 오후 03:30:45"))
  ]
  
  // Test locale-specific currency formatting
  let locale_currencies = [
    ("en_US.currency", azimuth::StringValue("$1,234.56")),
    ("de_DE.currency", azimuth::StringValue("1.234,56 €")),
    ("fr_FR.currency", azimuth::StringValue("1 234,56 €")),
    ("zh_CN.currency", azimuth::StringValue("¥1,234.56")),
    ("ja_JP.currency", azimuth::StringValue("￥1,235")),
    ("ru_RU.currency", azimuth::StringValue("1 234,56 ₽")),
    ("ar_SA.currency", azimuth::StringValue("١٬٢٣٤٫٥٦ ر.س")),
    ("hi_IN.currency", azimuth::StringValue("₹1,234.56"))
  ]
  
  // Set all locale-specific attributes
  for (key, value) in locale_numbers {
    azimuth::Attributes::set(attrs, key, value)
  }
  for (key, value) in locale_dates {
    azimuth::Attributes::set(attrs, key, value)
  }
  for (key, value) in locale_currencies {
    azimuth::Attributes::set(attrs, key, value)
  }
  
  // Verify all locale-specific attributes
  let all_locale_values = locale_numbers + locale_dates + locale_currencies
  for (key, expected_value) in all_locale_values {
    let actual_value = azimuth::Attributes::get(attrs, key)
    assert_eq(actual_value, Some(expected_value))
  }
}

// Test 4: Right-to-left (RTL) language support
pub test "right-to-left (RTL) language support" {
  let attrs = azimuth::Attributes::new()
  
  // Test RTL languages
  let rtl_text_samples = [
    // Arabic
    ("arabic.greeting", azimuth::StringValue("السلام عليكم")),
    ("arabic.sentence", azimuth::StringValue("مرحبا بكم في نظام المراقبة الخاص بنا")),
    ("arabic.mixed", azimuth::StringValue("النظام Azimuth يعمل بشكل جيد")),
    
    // Hebrew
    ("hebrew.greeting", azimuth::StringValue("שלום עולם")),
    ("hebrew.sentence", azimuth::StringValue("ברוכים הבאים למערכת המעקב שלנו")),
    ("hebrew.mixed", azimuth::StringValue("המערכת Azimuth עובדת היטב")),
    
    // Persian/Farsi
    ("persian.greeting", azimuth::StringValue("سلام دنیا")),
    ("persian.sentence", azimuth::StringValue("به سیستم نظارت ما خوش آمدید")),
    ("persian.mixed", azimuth::StringValue("سیستم Azimuth به خوبی کار می‌کند")),
    
    // Urdu
    ("urdu.greeting", azimuth::StringValue("ہیلو دنیا")),
    ("urdu.sentence", azimuth::StringValue("ہمارے نگرانی نظام میں خوش آمدید")),
    ("urdu.mixed", azimuth::StringValue("Azimuth نظام اچھی طرح کام کر رہا ہے"))
  ]
  
  // Set RTL text attributes
  for (key, value) in rtl_text_samples {
    azimuth::Attributes::set(attrs, key, value)
  }
  
  // Verify RTL text is preserved correctly
  for (key, expected_value) in rtl_text_samples {
    let actual_value = azimuth::Attributes::get(attrs, key)
    assert_eq(actual_value, Some(expected_value))
  }
  
  // Test RTL/LTR mixed content
  let mixed_directional_text = [
    ("mixed.en_ar", azimuth::StringValue("Hello العالم")),
    ("mixed.ar_en", azimuth::StringValue("مرحبا World")),
    ("mixed.he_en_ja", azimuth::StringValue("שלום World こんにちは")),
    ("mixed.complex", azimuth::StringValue("System: نظام المراقبة - Status: פעיל - User: ユーザー"))
  ]
  
  for (key, value) in mixed_directional_text {
    azimuth::Attributes::set(attrs, key, value)
  }
  
  for (key, expected_value) in mixed_directional_text {
    let actual_value = azimuth::Attributes::get(attrs, key)
    assert_eq(actual_value, Some(expected_value))
  }
}

// Test 5: Timezone and datetime internationalization
pub test "timezone and datetime internationalization" {
  let attrs = azimuth::Attributes::new()
  
  // Test different timezone representations
  let timezone_samples = [
    ("timezone.utc", azimuth::StringValue("2025-01-15T15:30:45Z")),
    ("timezone.est", azimuth::StringValue("2025-01-15T10:30:45-05:00")),
    ("timezone.pst", azimuth::StringValue("2025-01-15T07:30:45-08:00")),
    ("timezone.cet", azimuth::StringValue("2025-01-15T16:30:45+01:00")),
    ("timezone.jst", azimuth::StringValue("2025-01-16T00:30:45+09:00")),
    ("timezone.ist", azimuth::StringValue("2025-01-15T21:00:45+05:30")),
    ("timezone.cst", azimuth::StringValue("2025-01-15T23:30:45+08:00")),
    ("timezone.ms", azimuth::StringValue("2025-01-15T17:30:45+03:00"))
  ]
  
  // Test locale-specific date formats
  let locale_date_formats = [
    ("en_US.short_date", azimuth::StringValue("01/15/25")),
    ("en_US.long_date", azimuth::StringValue("January 15, 2025")),
    ("en_US.full_date", azimuth::StringValue("Wednesday, January 15, 2025")),
    
    ("de_DE.short_date", azimuth::StringValue("15.01.25")),
    ("de_DE.long_date", azimuth::StringValue("15. Januar 2025")),
    ("de_DE.full_date", azimuth::StringValue("Mittwoch, 15. Januar 2025")),
    
    ("fr_FR.short_date", azimuth::StringValue("15/01/2025")),
    ("fr_FR.long_date", azimuth::StringValue("15 janvier 2025")),
    ("fr_FR.full_date", azimuth::StringValue("mercredi 15 janvier 2025")),
    
    ("zh_CN.short_date", azimuth::StringValue("2025/1/15")),
    ("zh_CN.long_date", azimuth::StringValue("2025年1月15日")),
    ("zh_CN.full_date", azimuth::StringValue("2025年1月15日 星期三")),
    
    ("ja_JP.short_date", azimuth::StringValue("2025/01/15")),
    ("ja_JP.long_date", azimuth::StringValue("2025年1月15日")),
    ("ja_JP.full_date", azimuth::StringValue("2025年1月15日(水曜日)")),
    
    ("ru_RU.short_date", azimuth::StringValue("15.01.2025")),
    ("ru_RU.long_date", azimuth::StringValue("15 января 2025 г.")),
    ("ru_RU.full_date", azimuth::StringValue("среда, 15 января 2025 г."))
  ]
  
  // Test locale-specific time formats
  let locale_time_formats = [
    ("en_US.time_12h", azimuth::StringValue("3:30:45 PM")),
    ("en_US.time_24h", azimuth::StringValue("15:30:45")),
    
    ("de_DE.time_24h", azimuth::StringValue("15:30:45")),
    ("de_DE.time_12h", azimuth::StringValue("3:30:45 PM")),
    
    ("fr_FR.time_24h", azimuth::StringValue("15:30:45")),
    ("ja_JP.time_24h", azimuth::StringValue("15:30:45")),
    ("zh_CN.time_24h", azimuth::StringValue("15:30:45")),
    ("ru_RU.time_24h", azimuth::StringValue("15:30:45")),
    
    ("ko_KR.time_12h", azimuth::StringValue("오후 3:30:45")),
    ("ko_KR.time_24h", azimuth::StringValue("15:30:45")),
    
    ("ar_SA.time_12h", azimuth::StringValue("03:30:45 م")),
    ("ar_SA.time_24h", azimuth::StringValue("15:30:45"))
  ]
  
  // Set all timezone and datetime attributes
  for (key, value) in timezone_samples {
    azimuth::Attributes::set(attrs, key, value)
  }
  for (key, value) in locale_date_formats {
    azimuth::Attributes::set(attrs, key, value)
  }
  for (key, value) in locale_time_formats {
    azimuth::Attributes::set(attrs, key, value)
  }
  
  // Verify all datetime attributes
  let all_datetime_values = timezone_samples + locale_date_formats + locale_time_formats
  for (key, expected_value) in all_datetime_values {
    let actual_value = azimuth::Attributes::get(attrs, key)
    assert_eq(actual_value, Some(expected_value))
  }
}

// Test 6: Internationalization in span and trace names
pub test "internationalization in span and trace names" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "i18n-tracer")
  
  // Test span names in different languages
  let i18n_span_names = [
    ("en", "user_authentication"),
    ("zh", "用户身份验证"),
    ("es", "autenticación_usuario"),
    ("fr", "authentification_utilisateur"),
    ("de", "benutzer_authentifizierung"),
    ("ja", "ユーザー認証"),
    ("ko", "사용자_인증"),
    ("ru", "аутентификация_пользователя"),
    ("ar", "مصادقة_المستخدم"),
    ("hi", "उपयोगकर्ता_प्रमाणीकरण")
  ]
  
  // Create spans with internationalized names
  let spans = []
  for (lang, name) in i18n_span_names {
    let span = azimuth::Tracer::start_span(tracer, name)
    spans.push((lang, span))
  }
  
  // Verify all spans were created with correct names
  for (expected_lang, expected_name) in i18n_span_names {
    let found = false
    for (actual_lang, span) in spans {
      if actual_lang == expected_lang {
        assert_eq(azimuth::Span::name(span), expected_name)
        found = true
        break
      }
    }
    assert_true(found, "Span for language " + expected_lang + " not found")
  }
  
  // Test span attributes with internationalized content
  for (lang, span) in spans {
    let attrs = azimuth::Attributes::new()
    
    // Add language-specific attributes
    match lang {
      "en" => {
        azimuth::Attributes::set(attrs, "error.message", azimuth::StringValue("Invalid credentials"))
        azimuth::Attributes::set(attrs, "user.country", azimuth::StringValue("United States"))
      }
      "zh" => {
        azimuth::Attributes::set(attrs, "error.message", azimuth::StringValue("凭据无效"))
        azimuth::Attributes::set(attrs, "user.country", azimuth::StringValue("中国"))
      }
      "es" => {
        azimuth::Attributes::set(attrs, "error.message", azimuth::StringValue("Credenciales inválidas"))
        azimuth::Attributes::set(attrs, "user.country", azimuth::StringValue("España"))
      }
      "ja" => {
        azimuth::Attributes::set(attrs, "error.message", azimuth::StringValue("認証情報が無効です"))
        azimuth::Attributes::set(attrs, "user.country", azimuth::StringValue("日本"))
      }
      "ar" => {
        azimuth::Attributes::set(attrs, "error.message", azimuth::StringValue("بيانات الاعتماد غير صالحة"))
        azimuth::Attributes::set(attrs, "user.country", azimuth::StringValue("المملكة العربية السعودية"))
      }
      _ => {
        azimuth::Attributes::set(attrs, "error.message", azimuth::StringValue("Authentication failed"))
        azimuth::Attributes::set(attrs, "user.country", azimuth::StringValue("Unknown"))
      }
    }
    
    // Verify attributes were set
    let error_msg = azimuth::Attributes::get(attrs, "error.message")
    let user_country = azimuth::Attributes::get(attrs, "user.country")
    assert_true(error_msg.is_some())
    assert_true(user_country.is_some())
  }
}

// Test 7: Internationalization in metrics and instrument names
pub test "internationalization in metrics and instrument names" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "i18n-meter")
  
  // Test metric names in different languages
  let i18n_metric_names = [
    ("en", "http_requests_total"),
    ("zh", "http_请求总数"),
    ("es", "solicitudes_http_total"),
    ("fr", "total_requêtes_http"),
    ("de", "http_anfragen_gesamt"),
    ("ja", "httpリクエスト合計"),
    ("ko", "http_요청_총계"),
    ("ru", "всего_http_запросов"),
    ("ar", "إجمالي_طلبات_http"),
    ("hi", "_http_अनुरोध_कुल")
  ]
  
  // Create counters with internationalized names
  let counters = []
  for (lang, name) in i18n_metric_names {
    let counter = azimuth::Meter::create_counter(meter, name)
    counters.push((lang, counter))
  }
  
  // Verify all counters were created with correct names
  for (expected_lang, expected_name) in i18n_metric_names {
    let found = false
    for (actual_lang, counter) in counters {
      if actual_lang == expected_lang {
        assert_eq(counter.name, expected_name)
        found = true
        break
      }
    }
    assert_true(found, "Counter for language " + expected_lang + " not found")
  }
  
  // Test metric descriptions in different languages
  let counter = counters[0].1  // Get first counter
  let descriptions = [
    ("en", "Total number of HTTP requests"),
    ("zh", "HTTP请求的总数"),
    ("es", "Número total de solicitudes HTTP"),
    ("fr", "Nombre total de requêtes HTTP"),
    ("de", "Gesamtzahl der HTTP-Anfragen"),
    ("ja", "HTTPリクエストの総数"),
    ("ko", "HTTP 요청의 총 수"),
    ("ru", "Общее количество HTTP-запросов"),
    ("ar", "إجمالي عدد طلبات HTTP"),
    ("hi", "HTTP अनुरोधों की कुल संख्या")
  ]
  
  // Create attributes with internationalized descriptions
  let attrs = azimuth::Attributes::new()
  for (lang, description) in descriptions {
    let key = "metric.description." + lang
    azimuth::Attributes::set(attrs, key, azimuth::StringValue(description))
  }
  
  // Verify all descriptions were stored
  for (lang, expected_description) in descriptions {
    let key = "metric.description." + lang
    let actual_description = azimuth::Attributes::get(attrs, key)
    assert_eq(actual_description, Some(azimuth::StringValue(expected_description)))
  }
}

// Test 8: Resource attributes with international content
pub test "resource attributes with international content" {
  let resource = azimuth::Resource::new()
  
  // Test resource attributes in different languages
  let i18n_resource_attrs = [
    // Service information in multiple languages
    ("service.name.en", azimuth::StringValue("User Authentication Service")),
    ("service.name.zh", azimuth::StringValue("用户身份验证服务")),
    ("service.name.es", azimuth::StringValue("Servicio de Autenticación de Usuario")),
    ("service.name.fr", azimuth::StringValue("Service d'Authentification Utilisateur")),
    ("service.name.ja", azimuth::StringValue("ユーザー認証サービス")),
    ("service.name.ar", azimuth::StringValue("خدمة مصادقة المستخدم")),
    
    // Service descriptions
    ("service.description.en", azimuth::StringValue("Handles user authentication and authorization")),
    ("service.description.zh", azimuth::StringValue("处理用户身份验证和授权")),
    ("service.description.es", azimuth::StringValue("Maneja la autenticación y autorización de usuarios")),
    ("service.description.fr", azimuth::StringValue("Gère l'authentification et l'autorisation des utilisateurs")),
    ("service.description.ja", azimuth::StringValue("ユーザーの認証と認可を処理します")),
    ("service.description.ar", azimuth::StringValue("يتعامل مع مصادقة المستخدم والتفويض")),
    
    // Location information
    ("location.country.en", azimuth::StringValue("United States")),
    ("location.country.zh", azimuth::StringValue("美国")),
    ("location.country.es", azimuth::StringValue("Estados Unidos")),
    ("location.country.fr", azimuth::StringValue("États-Unis")),
    ("location.country.ja", azimuth::StringValue("アメリカ合衆国")),
    ("location.country.ar", azimuth::StringValue("الولايات المتحدة الأمريكية")),
    
    ("location.city.en", azimuth::StringValue("New York")),
    ("location.city.zh", azimuth::StringValue("纽约")),
    ("location.city.es", azimuth::StringValue("Nueva York")),
    ("location.city.fr", azimuth::StringValue("New York")),
    ("location.city.ja", azimuth::StringValue("ニューヨーク")),
    ("location.city.ar", azimuth::StringValue("نيويورك")),
    
    // Team and organization information
    ("team.name.en", azimuth::StringValue("Platform Engineering Team")),
    ("team.name.zh", azimuth::StringValue("平台工程团队")),
    ("team.name.es", azimuth::StringValue("Equipo de Ingeniería de Plataforma")),
    ("team.name.fr", azimuth::StringValue("Équipe d'Ingénierie Plateforme")),
    ("team.name.ja", azimuth::StringValue("プラットフォームエンジニアリングチーム")),
    ("team.name.ar", azimuth::StringValue("فريق هندسة المنصات")),
    
    ("organization.name.en", azimuth::StringValue("Global Technology Corporation")),
    ("organization.name.zh", azimuth::StringValue("全球技术公司")),
    ("organization.name.es", azimuth::StringValue("Corporación Tecnológica Global")),
    ("organization.name.fr", azimuth::StringValue("Société Technologique Mondiale")),
    ("organization.name.ja", azimuth::StringValue("グローバルテクノロジー株式会社")),
    ("organization.name.ar", azimuth::StringValue("شركة التكنولوجيا العالمية"))
  ]
  
  // Set all internationalized resource attributes
  let resource_with_i18n = azimuth::Resource::with_attributes(resource, i18n_resource_attrs)
  
  // Verify all internationalized attributes were stored correctly
  for (key, expected_value) in i18n_resource_attrs {
    let actual_value = azimuth::Resource::get_attribute(resource_with_i18n, key)
    assert_eq(actual_value, Some(expected_value))
  }
  
  // Test resource merging with internationalized content
  let additional_i18n_attrs = [
    ("service.version", azimuth::StringValue("2.1.0")),
    ("deployment.region.en", azimuth::StringValue("North America")),
    ("deployment.region.zh", azimuth::StringValue("北美")),
    ("deployment.region.es", azimuth::StringValue("América del Norte")),
    ("deployment.region.fr", azimuth::StringValue("Amérique du Nord")),
    ("deployment.region.ja", azimuth::StringValue("北米")),
    ("deployment.region.ar", azimuth::StringValue("أمريكا الشمالية"))
  ]
  
  let additional_resource = azimuth::Resource::with_attributes(resource, additional_i18n_attrs)
  let merged_resource = azimuth::Resource::merge(resource_with_i18n, additional_resource)
  
  // Verify merged resource contains all attributes
  let all_i18n_attrs = i18n_resource_attrs + additional_i18n_attrs
  for (key, expected_value) in all_i18n_attrs {
    let actual_value = azimuth::Resource::get_attribute(merged_resource, key)
    assert_eq(actual_value, Some(expected_value))
  }
}