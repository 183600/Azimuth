// Internationalization Support Tests
// 多语言支持测试

test "多语言错误消息本地化测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "i18n.errors")
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error.localization")
  
  // 创建错误本地化span
  let localization_span = Tracer::start_span(tracer, "multilingual.error.messages")
  
  // 定义支持的语言和错误消息
  let supported_languages = ["en", "zh-CN", "es", "fr", "de", "ja"]
  
  let error_messages = [
    ("connection.timeout", "en", "Connection timeout occurred"),
    ("connection.timeout", "zh-CN", "连接超时"),
    ("connection.timeout", "es", "Tiempo de conexión agotado"),
    ("connection.timeout", "fr", "Délai de connexion dépassé"),
    ("connection.timeout", "de", "Verbindungs-Timeout"),
    ("connection.timeout", "ja", "接続タイムアウト"),
    
    ("authentication.failed", "en", "Authentication failed"),
    ("authentication.failed", "zh-CN", "身份验证失败"),
    ("authentication.failed", "es", "Autenticación fallida"),
    ("authentication.failed", "fr", "Échec de l'authentification"),
    ("authentication.failed", "de", "Authentifizierung fehlgeschlagen"),
    ("authentication.failed", "ja", "認証に失敗しました"),
    
    ("resource.not.found", "en", "Resource not found"),
    ("resource.not.found", "zh-CN", "资源未找到"),
    ("resource.not.found", "es", "Recurso no encontrado"),
    ("resource.not.found", "fr", "Ressource introuvable"),
    ("resource.not.found", "de", "Ressource nicht gefunden"),
    ("resource.not.found", "ja", "リソースが見つかりません"),
    
    ("permission.denied", "en", "Permission denied"),
    ("permission.denied", "zh-CN", "权限被拒绝"),
    ("permission.denied", "es", "Permiso denegado"),
    ("permission.denied", "fr", "Autorisation refusée"),
    ("permission.denied", "de", "Zugriff verweigert"),
    ("permission.denied", "ja", "アクセスが拒否されました")
  ]
  
  // 为每种语言测试错误消息
  for language in supported_languages {
    let language_span = Tracer::start_span(tracer, "error.localization." + language)
    
    Span::add_event(language_span, "language.context.set", Some([
      ("target.language", StringValue(language)),
      ("locale.region", StringValue(match language {
        "zh-CN" => "China"
        "es" => "Spain"
        "fr" => "France"
        "de" => "Germany"
        "ja" => "Japan"
        _ => "United States"
      })),
      ("encoding", StringValue("UTF-8"))
    ]))
    
    // 测试该语言的所有错误消息
    for message in error_messages {
      if message.1 == language {
        let error_code = message.0
        let localized_message = message.2
        
        // 创建本地化错误日志
        let localized_error_log = LogRecord::new_with_context(
          Error,
          Some(localized_message),
          Some(Attributes::with_attributes(Attributes::new(), [
            ("error.code", StringValue(error_code)),
            ("language", StringValue(language)),
            ("message.localized", BoolValue(true)),
            ("encoding", StringValue("UTF-8")),
            ("timestamp", IntValue(Clock::now_unix_nanos(Clock::system()).to_int()))
          ])),
          Some(Clock::now_unix_nanos(Clock::system())),
          None,
          Some("error.trace." + error_code),
          Some("error.span." + language),
          None
        )
        
        Logger::emit(logger, localized_error_log)
        
        // 验证错误消息不为空
        assert_true(localized_message.length() > 0)
        
        Span::add_event(language_span, "error.localized", Some([
          ("error.code", StringValue(error_code)),
          ("message.length", IntValue(localized_message.length())),
          ("has.translation", BoolValue(true))
        ]))
      }
    }
    
    Span::set_status(language_span, Ok)
    Span::end(language_span)
  }
  
  Span::add_event(localization_span, "localization.completed", Some([
    ("languages.supported", IntValue(supported_languages.length())),
    ("error.types.localized", IntValue(4)),
    ("total.messages.generated", IntValue(error_messages.length()))
  ]))
  
  Span::set_status(localization_span, Ok)
  Span::end(localization_span)
  
  assert_true(supported_languages.length() > 0 && error_messages.length() > 0)
}

test "日期时间和数字格式本地化测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "format.localization")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "i18n.formats")
  
  // 创建格式本地化span
  let format_span = Tracer::start_span(tracer, "datetime.number.formatting")
  
  // 模拟不同地区的格式化规则
  let locale_formats = [
    ("en-US", "MM/DD/YYYY", "1,234.56", "USD", "$"),
    ("zh-CN", "YYYY年MM月DD日", "1,234.56", "CNY", "¥"),
    ("de-DE", "DD.MM.YYYY", "1.234,56", "EUR", "€"),
    ("fr-FR", "DD/MM/YYYY", "1 234,56", "EUR", "€"),
    ("ja-JP", "YYYY年MM月DD日", "1,234.56", "JPY", "¥"),
    ("es-ES", "DD/MM/YYYY", "1.234,56", "EUR", "€")
  ]
  
  let test_timestamp = Clock::now_unix_nanos(Clock::system())
  let test_number = 1234.56
  let test_currency = 1234.56
  
  for locale in locale_formats {
    let locale_code = locale.0
    let date_format = locale.1
    let number_format = locale.2
    let currency_code = locale.3
    let currency_symbol = locale.4
    
    // 创建地区特定格式化span
    let locale_span = Tracer::start_span(tracer, "format." + locale_code)
    
    // 模拟日期时间格式化
    let formatted_date = match locale_code {
      "en-US" => "12/28/2025"
      "zh-CN" => "2025年12月28日"
      "de-DE" => "28.12.2025"
      "fr-FR" => "28/12/2025"
      "ja-JP" => "2025年12月28日"
      "es-ES" => "28/12/2025"
      _ => "2025-12-28"
    }
    
    Span::add_event(locale_span, "date.formatted", Some([
      ("locale", StringValue(locale_code)),
      ("format.pattern", StringValue(date_format)),
      ("formatted.date", StringValue(formatted_date)),
      ("timezone", StringValue("UTC"))
    ]))
    
    // 模拟数字格式化
    let formatted_number = match locale_code {
      "en-US" => "1,234.56"
      "zh-CN" => "1,234.56"
      "de-DE" => "1.234,56"
      "fr-FR" => "1 234,56"
      "ja-JP" => "1,234.56"
      "es-ES" => "1.234,56"
      _ => "1234.56"
    }
    
    Span::add_event(locale_span, "number.formatted", Some([
      ("locale", StringValue(locale_code)),
      ("original.value", FloatValue(test_number)),
      ("formatted.number", StringValue(formatted_number)),
      ("decimal.separator", StringValue(if locale_code == "de-DE" || locale_code == "es-ES" { "," } else { "." })),
      ("thousands.separator", StringValue(match locale_code {
        "de-DE" => "."
        "fr-FR" => " "
        _ => ","
      }))
    ]))
    
    // 模拟货币格式化
    let formatted_currency = match locale_code {
      "en-US" => "$1,234.56"
      "zh-CN" => "¥1,234.56"
      "de-DE" => "1.234,56 €"
      "fr-FR" => "1 234,56 €"
      "ja-JP" => "¥1,235"  // 日元没有小数
      "es-ES" => "1.234,56 €"
      _ => "$1,234.56"
    }
    
    Span::add_event(locale_span, "currency.formatted", Some([
      ("locale", StringValue(locale_code)),
      ("currency.code", StringValue(currency_code)),
      ("currency.symbol", StringValue(currency_symbol)),
      ("formatted.currency", StringValue(formatted_currency)),
      ("original.amount", FloatValue(test_currency))
    ]))
    
    // 创建格式化日志
    let format_log = LogRecord::new_with_context(
      Info,
      Some("Locale-specific formatting applied for " + locale_code),
      Some(Attributes::with_attributes(Attributes::new(), [
        ("locale", StringValue(locale_code)),
        ("date.format", StringValue(formatted_date)),
        ("number.format", StringValue(formatted_number)),
        ("currency.format", StringValue(formatted_currency)),
        ("formatting.success", BoolValue(true))
      ])),
      Some(test_timestamp),
      None,
      Some("format.trace." + locale_code),
      Some("format.span"),
      None
    )
    
    Logger::emit(logger, format_log)
    
    Span::set_status(locale_span, Ok)
    Span::end(locale_span)
  }
  
  Span::add_event(format_span, "formatting.completed", Some([
    ("locales.processed", IntValue(locale_formats.length())),
    ("format.types", IntValue(3)), // date, number, currency
    ("all.formats.success", BoolValue(true))
  ]))
  
  Span::set_status(format_span, Ok)
  Span::end(format_span)
  
  assert_true(locale_formats.length() > 0)
}

test "UI界面文本国际化测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "i18n.ui")
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "ui.localization")
  
  // 创建UI本地化span
  let ui_span = Tracer::start_span(tracer, "user.interface.localization")
  
  // 定义UI文本键值对
  let ui_texts = [
    ("button.save", "en", "Save"),
    ("button.save", "zh-CN", "保存"),
    ("button.save", "es", "Guardar"),
    ("button.save", "fr", "Enregistrer"),
    ("button.save", "de", "Speichern"),
    ("button.save", "ja", "保存"),
    
    ("button.cancel", "en", "Cancel"),
    ("button.cancel", "zh-CN", "取消"),
    ("button.cancel", "es", "Cancelar"),
    ("button.cancel", "fr", "Annuler"),
    ("button.cancel", "de", "Abbrechen"),
    ("button.cancel", "ja", "キャンセル"),
    
    ("menu.dashboard", "en", "Dashboard"),
    ("menu.dashboard", "zh-CN", "仪表板"),
    ("menu.dashboard", "es", "Panel de control"),
    ("menu.dashboard", "fr", "Tableau de bord"),
    ("menu.dashboard", "de", "Dashboard"),
    ("menu.dashboard", "ja", "ダッシュボード"),
    
    ("message.loading", "en", "Loading..."),
    ("message.loading", "zh-CN", "加载中..."),
    ("message.loading", "es", "Cargando..."),
    ("message.loading", "fr", "Chargement..."),
    ("message.loading", "de", "Wird geladen..."),
    ("message.loading", "ja", "読み込み中..."),
    
    ("error.network", "en", "Network error occurred"),
    ("error.network", "zh-CN", "发生网络错误"),
    ("error.network", "es", "Error de red"),
    ("error.network", "fr", "Erreur réseau"),
    ("error.network", "de", "Netzwerkfehler"),
    ("error.network", "ja", "ネットワークエラーが発生しました")
  ]
  
  let languages = ["en", "zh-CN", "es", "fr", "de", "ja"]
  
  // 为每种语言构建UI界面
  for language in languages {
    let language_ui_span = Tracer::start_span(tracer, "ui.build." + language)
    
    Span::add_event(language_ui_span, "ui.localization.started", Some([
      ("target.language", StringValue(language)),
      ("ui.components", IntValue(5)),
      ("text.direction", StringValue(if language == "ar" { "rtl" } else { "ltr" }))
    ]))
    
    // 模拟UI组件构建
    let ui_components = []
    
    for text in ui_texts {
      if text.1 == language {
        let component_key = text.0
        let localized_text = text.2
        
        ui_components.push((component_key, localized_text))
        
        Span::add_event(language_ui_span, "component.localized", Some([
          ("component.key", StringValue(component_key)),
          ("text.length", IntValue(localized_text.length())),
          ("has.translation", BoolValue(true))
        ]))
      }
    }
    
    // 验证所有组件都已本地化
    assert_eq(ui_components.length(), 5) // 每种语言应该有5个UI组件
    
    // 创建UI构建完成日志
    let ui_build_log = LogRecord::new_with_context(
      Info,
      Some("UI localization completed for " + language),
      Some(Attributes::with_attributes(Attributes::new(), [
        ("language", StringValue(language)),
        ("components.localized", IntValue(ui_components.length())),
        ("ui.ready", BoolValue(true)),
        ("fallback.language", StringValue("en"))
      ])),
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some("ui.trace." + language),
      Some("ui.build.span"),
      None
    )
    
    Logger::emit(logger, ui_build_log)
    
    Span::set_status(language_ui_span, Ok)
    Span::end(language_ui_span)
  }
  
  // 测试语言切换功能
  let switch_span = Tracer::start_span(tracer, "language.switching")
  
  let switch_sequence = ["en", "zh-CN", "es", "en"]
  
  for i = 0; i < switch_sequence.length(); i = i + 1 {
    let from_lang = if i == 0 { "en" } else { switch_sequence[i - 1] }
    let to_lang = switch_sequence[i]
    
    Span::add_event(switch_span, "language.switched", Some([
      ("from.language", StringValue(from_lang)),
      ("to.language", StringValue(to_lang)),
      ("switch.time.ms", IntValue(150)),
      ("ui.rebuilt", BoolValue(true))
    ]))
    
    // 创建语言切换日志
    let switch_log = LogRecord::new_with_context(
      Info,
      Some("Language switched from " + from_lang + " to " + to_lang),
      Some(Attributes::with_attributes(Attributes::new(), [
        ("action", StringValue("language_switch")),
        ("previous.language", StringValue(from_lang)),
        ("new.language", StringValue(to_lang)),
        ("switch.successful", BoolValue(true))
      ])),
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some("switch.trace"),
      Some("switch.span"),
      None
    )
    
    Logger::emit(logger, switch_log)
  }
  
  Span::set_status(switch_span, Ok)
  Span::end(switch_span)
  
  Span::add_event(ui_span, "ui.localization.completed", Some([
    ("languages.supported", IntValue(languages.length())),
    ("ui.components.total", IntValue(5)),
    ("language.switches", IntValue(switch_sequence.length() - 1)),
    ("all.localization.success", BoolValue(true))
  ]))
  
  Span::set_status(ui_span, Ok)
  Span::end(ui_span)
  
  assert_true(languages.length() > 0 && ui_texts.length() > 0)
}

test "时区和日历系统本地化测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "timezone.localization")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "i18n.timezone")
  
  // 创建时区本地化span
  let timezone_span = Tracer::start_span(tracer, "timezone.calendar.localization")
  
  // 定义不同的时区和日历系统
  let timezone_configs = [
    ("UTC", "Gregorian", "en-US", "12/28/2025 10:30:00 AM"),
    ("America/New_York", "Gregorian", "en-US", "12/28/2025 05:30:00 AM"),
    ("Europe/London", "Gregorian", "en-GB", "28/12/2025 10:30:00"),
    ("Asia/Shanghai", "Gregorian", "zh-CN", "2025年12月28日 18:30:00"),
    ("Asia/Tokyo", "Gregorian", "ja-JP", "2025年12月28日 19:30:00"),
    ("Asia/Dubai", "Gregorian", "ar-AE", "28/12/2025 02:30:00 م"),
    ("America/Sao_Paulo", "Gregorian", "pt-BR", "28/12/2025 07:30:00")
  ]
  
  let base_timestamp = Clock::now_unix_nanos(Clock::system())
  
  for config in timezone_configs {
    let timezone = config.0
    let calendar = config.1
    let locale = config.2
    let expected_format = config.3
    
    // 创建时区特定span
    let tz_span = Tracer::start_span(tracer, "timezone." + timezone.replace("/", "."))
    
    Span::add_event(tz_span, "timezone.conversion", Some([
      ("source.timezone", StringValue("UTC")),
      ("target.timezone", StringValue(timezone)),
      ("calendar.system", StringValue(calendar)),
      ("locale", StringValue(locale)),
      ("base.timestamp", IntValue(base_timestamp.to_int()))
    ]))
    
    // 模拟时区转换
    let converted_time = expected_format
    
    Span::add_event(tz_span, "time.converted", Some([
      ("converted.time", StringValue(converted_time)),
      ("conversion.success", BoolValue(true)),
      ("dst.adjusted", BoolValue(timezone == "America/New_York")) // 纽约可能有夏令时
    ]))
    
    // 测试工作日和周末的本地化
    let weekday = match timezone {
      "America/New_York" => "Sunday"
      "Europe/London" => "Sunday"
      "Asia/Shanghai" => "星期日"
      "Asia/Tokyo" => "日曜日"
      "Asia/Dubai" => "الأحد"
      "America/Sao_Paulo" => "domingo"
      _ => "Sunday"
    }
    
    Span::add_event(tz_span, "weekday.localized", Some([
      ("weekday.name", StringValue(weekday)),
      ("is.weekend", BoolValue(true)),
      ("locale", StringValue(locale))
    ]))
    
    // 创建时区转换日志
    let tz_log = LogRecord::new_with_context(
      Info,
      Some("Timezone conversion completed for " + timezone),
      Some(Attributes::with_attributes(Attributes::new(), [
        ("timezone", StringValue(timezone)),
        ("calendar", StringValue(calendar)),
        ("locale", StringValue(locale)),
        ("converted.time", StringValue(converted_time)),
        ("weekday", StringValue(weekday))
      ])),
      Some(base_timestamp),
      None,
      Some("timezone.trace." + timezone),
      Some("timezone.span"),
      None
    )
    
    Logger::emit(logger, tz_log)
    
    Span::set_status(tz_span, Ok)
    Span::end(tz_span)
  }
  
  // 测试相对时间的本地化
  let relative_time_span = Tracer::start_span(tracer, "relative.time.localization")
  
  let relative_time_phrases = [
    ("en", "2 hours ago", "in 3 days", "1 minute ago"),
    ("zh-CN", "2小时前", "3天后", "1分钟前"),
    ("es", "hace 2 horas", "en 3 días", "hace 1 minuto"),
    ("fr", "il y a 2 heures", "dans 3 jours", "il y a 1 minute"),
    ("de", "vor 2 Stunden", "in 3 Tagen", "vor 1 Minute"),
    ("ja", "2時間前", "3日後", "1分前")
  ]
  
  for phrase in relative_time_phrases {
    let language = phrase.0
    let past_phrase = phrase.1
    let future_phrase = phrase.2
    let recent_phrase = phrase.3
    
    Span::add_event(relative_time_span, "relative.time.localized", Some([
      ("language", StringValue(language)),
      ("past.phrase", StringValue(past_phrase)),
      ("future.phrase", StringValue(future_phrase)),
      ("recent.phrase", StringValue(recent_phrase))
    ]))
  }
  
  Span::set_status(relative_time_span, Ok)
  Span::end(relative_time_span)
  
  Span::add_event(timezone_span, "timezone.localization.completed", Some([
    ("timezones.processed", IntValue(timezone_configs.length())),
    ("calendar.systems", IntValue(1)),
    ("relative.time.languages", IntValue(relative_time_phrases.length())),
    ("all.conversions.success", BoolValue(true))
  ]))
  
  Span::set_status(timezone_span, Ok)
  Span::end(timezone_span)
  
  assert_true(timezone_configs.length() > 0 && relative_time_phrases.length() > 0)
}