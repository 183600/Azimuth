// Azimuth Telemetry System - Internationalization Support Tests
// This file contains test cases for internationalization functionality

// Test 1: Locale Detection and Management
test "locale detection and management" {
  // Create a locale manager
  let locale_manager = LocaleManager::new()
  
  // Test default locale
  let default_locale = LocaleManager::get_default_locale(locale_manager)
  assert_eq(default_locale.language, "en")
  assert_eq(default_locale.region, "US")
  
  // Test setting locale
  let french_locale = Locale::new("fr", "FR")
  LocaleManager::set_current_locale(locale_manager, french_locale)
  
  let current_locale = LocaleManager::get_current_locale(locale_manager)
  assert_eq(current_locale.language, "fr")
  assert_eq(current_locale.region, "FR")
  
  // Test locale detection from system
  let system_locale = LocaleManager::detect_system_locale(locale_manager)
  assert_true(system_locale.language.length() == 2)
  assert_true(system_locale.region.length() == 2)
  
  // Test locale fallback
  LocaleManager::add_fallback_locale(locale_manager, Locale::new("en", "US"))
  
  // Try to set an unsupported locale
  let unsupported_locale = Locale::new("xx", "XX")
  LocaleManager::set_current_locale(locale_manager, unsupported_locale)
  
  // Should fall back to the default or fallback locale
  let fallback_locale = LocaleManager::get_current_locale(locale_manager)
  assert_true(fallback_locale.language == "en" || fallback_locale.language == "fr")
  
  // Test locale preferences
  let preferences = [Locale::new("zh", "CN"), Locale::new("en", "US"), Locale::new("fr", "FR")]
  LocaleManager::set_preferred_locales(locale_manager, preferences)
  
  let resolved_locale = LocaleManager::resolve_locale(locale_manager)
  assert_eq(resolved_locale.language, "zh")
  assert_eq(resolved_locale.region, "CN")
}

// Test 2: Message Localization
test "message localization" {
  // Create a message localizer
  let localizer = MessageLocalizer::new()
  
  // Add translations for different locales
  MessageLocalizer::add_translation(localizer, "en", "US", "telemetry.error.connection_failed", "Connection to telemetry server failed")
  MessageLocalizer::add_translation(localizer, "fr", "FR", "telemetry.error.connection_failed", "La connexion au serveur de télémétrie a échoué")
  MessageLocalizer::add_translation(localizer, "zh", "CN", "telemetry.error.connection_failed", "连接遥测服务器失败")
  MessageLocalizer::add_translation(localizer, "es", "ES", "telemetry.error.connection_failed", "Error de conexión al servidor de telemetría")
  
  // Test localization for different locales
  let en_message = MessageLocalizer::localize(localizer, "telemetry.error.connection_failed", Locale::new("en", "US"))
  assert_eq(en_message, "Connection to telemetry server failed")
  
  let fr_message = MessageLocalizer::localize(localizer, "telemetry.error.connection_failed", Locale::new("fr", "FR"))
  assert_eq(fr_message, "La connexion au serveur de télémétrie a échoué")
  
  let zh_message = MessageLocalizer::localize(localizer, "telemetry.error.connection_failed", Locale::new("zh", "CN"))
  assert_eq(zh_message, "连接遥测服务器失败")
  
  let es_message = MessageLocalizer::localize(localizer, "telemetry.error.connection_failed", Locale::new("es", "ES"))
  assert_eq(es_message, "Error de conexión al servidor de telemetría")
  
  // Test fallback to language-only locale
  let fr_ca_message = MessageLocalizer::localize(localizer, "telemetry.error.connection_failed", Locale::new("fr", "CA"))
  assert_eq(fr_ca_message, "La connexion au serveur de télémétrie a échoué") // Should fall back to fr-FR
  
  // Test fallback to default locale
  let unsupported_message = MessageLocalizer::localize(localizer, "telemetry.error.connection_failed", Locale::new("xx", "XX"))
  assert_eq(unsupported_message, "Connection to telemetry server failed") // Should fall back to en-US
  
  // Test message with parameters
  MessageLocalizer::add_translation(localizer, "en", "US", "telemetry.metric.value", "Metric {metric_name} has value {value}")
  MessageLocalizer::add_translation(localizer, "fr", "FR", "telemetry.metric.value", "La métrique {metric_name} a une valeur de {value}")
  
  let en_params = [("metric_name", "CPU Usage"), ("value", "75.5%")]
  let en_param_message = MessageLocalizer::localize_with_params(localizer, "telemetry.metric.value", Locale::new("en", "US"), en_params)
  assert_eq(en_param_message, "Metric CPU Usage has value 75.5%")
  
  let fr_params = [("metric_name", "Utilisation CPU"), ("value", "75,5%")]
  let fr_param_message = MessageLocalizer::localize_with_params(localizer, "telemetry.metric.value", Locale::new("fr", "FR"), fr_params)
  assert_eq(fr_param_message, "La métrique Utilisation CPU a une valeur de 75,5%")
}

// Test 3: Number and Date Formatting
test "number and date formatting" {
  // Create a formatter
  let formatter = LocaleFormatter::new()
  
  // Test number formatting for different locales
  let number = 1234567.89
  
  let us_formatted = LocaleFormatter::format_number(formatter, number, Locale::new("en", "US"))
  assert_eq(us_formatted, "1,234,567.89") // US format: comma for thousands, dot for decimal
  
  let fr_formatted = LocaleFormatter::format_number(formatter, number, Locale::new("fr", "FR"))
  assert_eq(fr_formatted, "1 234 567,89") // French format: space for thousands, comma for decimal
  
  let de_formatted = LocaleFormatter::format_number(formatter, number, Locale::new("de", "DE"))
  assert_eq(de_formatted, "1.234.567,89") // German format: dot for thousands, comma for decimal
  
  // Test percentage formatting
  let percentage = 0.755
  
  let us_percent = LocaleFormatter::format_percentage(formatter, percentage, Locale::new("en", "US"))
  assert_eq(us_percent, "75.5%")
  
  let fr_percent = LocaleFormatter::format_percentage(formatter, percentage, Locale::new("fr", "FR"))
  assert_eq(fr_percent, "75,5 %") // Note the space before % in French
  
  // Test currency formatting
  let currency = 1234.56
  
  let us_currency = LocaleFormatter::format_currency(formatter, currency, "USD", Locale::new("en", "US"))
  assert_eq(us_currency, "$1,234.56")
  
  let fr_currency = LocaleFormatter::format_currency(formatter, currency, "EUR", Locale::new("fr", "FR"))
  assert_eq(fr_currency, "1 234,56 €")
  
  let de_currency = LocaleFormatter::format_currency(formatter, currency, "EUR", Locale::new("de", "DE"))
  assert_eq(de_currency, "1.234,56 €")
  
  // Test date formatting
  let timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  let us_date = LocaleFormatter::format_date(formatter, timestamp, Locale::new("en", "US"))
  assert_eq(us_date, "1/1/2021") // US format: MM/DD/YYYY
  
  let fr_date = LocaleFormatter::format_date(formatter, timestamp, Locale::new("fr", "FR"))
  assert_eq(fr_date, "01/01/2021") // French format: DD/MM/YYYY
  
  let iso_date = LocaleFormatter::format_date(formatter, timestamp, Locale::new("en", "ISO"))
  assert_eq(iso_date, "2021-01-01") // ISO format: YYYY-MM-DD
  
  // Test time formatting
  let us_time = LocaleFormatter::format_time(formatter, timestamp, Locale::new("en", "US"))
  assert_eq(us_time, "12:00:00 AM") // US 12-hour format
  
  let fr_time = LocaleFormatter::format_time(formatter, timestamp, Locale::new("fr", "FR"))
  assert_eq(fr_time, "00:00:00") // French 24-hour format
  
  // Test date-time formatting
  let us_datetime = LocaleFormatter::format_datetime(formatter, timestamp, Locale::new("en", "US"))
  assert_eq(us_datetime, "1/1/2021, 12:00:00 AM")
  
  let fr_datetime = LocaleFormatter::format_datetime(formatter, timestamp, Locale::new("fr", "FR"))
  assert_eq(fr_datetime, "01/01/2021 à 00:00:00") // French format with "à" (at)
}

// Test 4: Right-to-Left Language Support
test "right-to-left language support" {
  // Create a direction manager
  let direction_manager = DirectionManager::new()
  
  // Test direction detection for different locales
  let en_direction = DirectionManager::get_text_direction(direction_manager, Locale::new("en", "US"))
  assert_eq(en_direction, LeftToRight)
  
  let ar_direction = DirectionManager::get_text_direction(direction_manager, Locale::new("ar", "SA"))
  assert_eq(ar_direction, RightToLeft)
  
  let he_direction = DirectionManager::get_text_direction(direction_manager, Locale::new("he", "IL"))
  assert_eq(he_direction, RightToLeft)
  
  let fa_direction = DirectionManager::get_text_direction(direction_manager, Locale::new("fa", "IR"))
  assert_eq(fa_direction, RightToLeft)
  
  // Test text wrapping with direction
  let english_text = "This is English text"
  let wrapped_en = DirectionManager::wrap_with_direction(direction_manager, english_text, Locale::new("en", "US"))
  assert_eq(wrapped_en, "<span dir=\"ltr\">This is English text</span>")
  
  let arabic_text = "هذا نص باللغة العربية"
  let wrapped_ar = DirectionManager::wrap_with_direction(direction_manager, arabic_text, Locale::new("ar", "SA"))
  assert_eq(wrapped_ar, "<span dir=\"rtl\">هذا نص باللغة العربية</span>")
  
  // Test mixed direction text
  let mixed_text = "English مع Arabic نص"
  let wrapped_mixed = DirectionManager::wrap_with_direction(direction_manager, mixed_text, Locale::new("ar", "SA"))
  assert_eq(wrapped_mixed, "<span dir=\"rtl\">English مع Arabic نص</span>")
  
  // Test auto-detection of text direction
  let detected_en = DirectionManager::auto_detect_direction(direction_manager, "This is English text")
  assert_eq(detected_en, LeftToRight)
  
  let detected_ar = DirectionManager::auto_detect_direction(direction_manager, "هذا نص باللغة العربية")
  assert_eq(detected_ar, RightToLeft)
  
  // Test layout mirroring for RTL
  let layout_properties = DirectionManager::get_layout_properties(direction_manager, Locale::new("ar", "SA"))
  assert_true(layout_properties.should_mirror_layout)
  assert_eq(layout_properties.text_align, "right")
  assert_eq(layout_properties.margin_start, "margin-right")
  assert_eq(layout_properties.margin_end, "margin-left")
  
  let ltr_layout = DirectionManager::get_layout_properties(direction_manager, Locale::new("en", "US"))
  assert_false(ltr_layout.should_mirror_layout)
  assert_eq(ltr_layout.text_align, "left")
  assert_eq(ltr_layout.margin_start, "margin-left")
  assert_eq(ltr_layout.margin_end, "margin-right")
}

// Test 5: Collation and Sorting
test "collation and sorting" {
  // Create a collator
  let collator = Collator::new()
  
  // Test sorting in different locales
  let words = ["apple", "banana", "cherry", "date"]
  
  let en_sorted = Collator::sort(collator, words, Locale::new("en", "US"))
  assert_eq(en_sorted, ["apple", "banana", "cherry", "date"]) // English alphabetical order
  
  // Test with accented characters
  let accented_words = ["café", "cane", "came", "côte"]
  
  let fr_sorted = Collator::sort(collator, accented_words, Locale::new("fr", "FR"))
  // In French, accent differences are considered after base letters
  assert_eq(fr_sorted, ["cane", "came", "café", "côte"])
  
  // Test with Chinese characters
  let chinese_words = ["苹果", "香蕉", "樱桃", "枣"]
  
  let zh_sorted = Collator::sort(collator, chinese_words, Locale::new("zh", "CN"))
  assert_eq(zh_sorted, ["枣", "苹果", "樱桃", "香蕉"]) // Sorted by stroke count or pinyin
  
  // Test with German umlauts
  let german_words = ["äpfel", "banana", "österreich", "uber"]
  
  let de_sorted = Collator::sort(collator, german_words, Locale::new("de", "DE"))
  // In German, ä and ö are treated as a and o for sorting
  assert_eq(de_sorted, ["äpfel", "banana", "österreich", "uber"])
  
  // Test case sensitivity
  let mixed_case_words = ["Apple", "banana", "Cherry", "date"]
  
  let case_sensitive = Collator::sort_with_options(collator, mixed_case_words, Locale::new("en", "US"), { case_sensitive: true })
  assert_eq(case_sensitive, ["Apple", "Cherry", "banana", "date"]) // Uppercase comes before lowercase
  
  let case_insensitive = Collator::sort_with_options(collator, mixed_case_words, Locale::new("en", "US"), { case_sensitive: false })
  assert_eq(case_insensitive, ["Apple", "banana", "Cherry", "date"]) // Case ignored for sorting
  
  // Test string comparison
  let en_comparison = Collator::compare(collator, "apple", "banana", Locale::new("en", "US"))
  assert_true(en_comparison < 0) // "apple" comes before "banana"
  
  let de_comparison = Collator::compare(collator, "äpfel", "apfel", Locale::new("de", "DE"))
  assert_eq(de_comparison, 0) // Treated as equal in German collation
}

// Test 6: Pluralization Rules
test "pluralization rules" {
  // Create a pluralizer
  let pluralizer = Pluralizer::new()
  
  // Test English pluralization (one/other)
  Pluralizer::add_rule(pluralizer, "en", "item", {
    "one": "item",
    "other": "items"
  })
  
  let en_one = Pluralizer::pluralize(pluralizer, "en", "item", 1)
  assert_eq(en_one, "item")
  
  let en_other = Pluralizer::pluralize(pluralizer, "en", "item", 2)
  assert_eq(en_other, "items")
  
  let en_zero = Pluralizer::pluralize(pluralizer, "en", "item", 0)
  assert_eq(en_zero, "items") // Zero uses "other" form in English
  
  // Test French pluralization (one/other, but zero uses "other")
  Pluralizer::add_rule(pluralizer, "fr", "élément", {
    "one": "élément",
    "other": "éléments"
  })
  
  let fr_one = Pluralizer::pluralize(pluralizer, "fr", "élément", 1)
  assert_eq(fr_one, "élément")
  
  let fr_other = Pluralizer::pluralize(pluralizer, "fr", "élément", 2)
  assert_eq(fr_other, "éléments")
  
  // Test Arabic pluralization (zero/one/two/few/many/other)
  Pluralizer::add_rule(pluralizer, "ar", "عنصر", {
    "zero": "عناصر",
    "one": "عنصر",
    "two": "عنصران",
    "few": "عناصر",
    "many": "عناصر",
    "other": "عناصر"
  })
  
  let ar_zero = Pluralizer::pluralize(pluralizer, "ar", "عنصر", 0)
  assert_eq(ar_zero, "عناصر")
  
  let ar_one = Pluralizer::pluralize(pluralizer, "ar", "عنصر", 1)
  assert_eq(ar_one, "عنصر")
  
  let ar_two = Pluralizer::pluralize(pluralizer, "ar", "عنصر", 2)
  assert_eq(ar_two, "عنصران")
  
  let ar_few = Pluralizer::pluralize(pluralizer, "ar", "عنصر", 5)
  assert_eq(ar_few, "عناصر")
  
  // Test Chinese pluralization (no plural forms)
  Pluralizer::add_rule(pluralizer, "zh", "项目", {
    "other": "项目"
  })
  
  let zh_one = Pluralizer::pluralize(pluralizer, "zh", "项目", 1)
  assert_eq(zh_one, "项目")
  
  let zh_other = Pluralizer::pluralize(pluralizer, "zh", "项目", 10)
  assert_eq(zh_other, "项目")
  
  // Test with parameters
  Pluralizer::add_rule_with_params(pluralizer, "en", "file", {
    "one": "{count} file",
    "other": "{count} files"
  })
  
  let en_one_params = [("count", "1")]
  let en_one_with_param = Pluralizer::pluralize_with_params(pluralizer, "en", "file", 1, en_one_params)
  assert_eq(en_one_with_param, "1 file")
  
  let en_other_params = [("count", "5")]
  let en_other_with_param = Pluralizer::pluralize_with_params(pluralizer, "en", "file", 5, en_other_params)
  assert_eq(en_other_with_param, "5 files")
}

// Test 7: Calendar and Time Zone Support
test "calendar and time zone support" {
  // Create a calendar manager
  let calendar_manager = CalendarManager::new()
  
  // Test different calendar systems
  let gregorian_date = CalendarManager::convert_to_calendar(
    calendar_manager,
    2021,
    1,
    15,
    Gregorian,
    Gregorian
  )
  assert_eq(gregorian_date.year, 2021)
  assert_eq(gregorian_date.month, 1)
  assert_eq(gregorian_date.day, 15)
  
  // Test Islamic calendar conversion
  let islamic_date = CalendarManager::convert_to_calendar(
    calendar_manager,
    2021,
    1,
    15,
    Gregorian,
    Islamic
  )
  // January 15, 2021 corresponds to a specific date in Islamic calendar
  assert_true(islamic_date.year > 1440) // Should be in the 1440s AH
  assert_true(islamic_date.month >= 1 && islamic_date.month <= 12)
  assert_true(islamic_date.day >= 1 && islamic_date.day <= 30)
  
  // Test Hebrew calendar conversion
  let hebrew_date = CalendarManager::convert_to_calendar(
    calendar_manager,
    2021,
    1,
    15,
    Gregorian,
    Hebrew
  )
  // Should be a valid Hebrew calendar date
  assert_true(hebrew_date.year > 5780) // Should be in the 5780s AM
  assert_true(hebrew_date.month >= 1 && hebrew_date.month <= 13) // Hebrew months can be up to 13 in leap years
  assert_true(hebrew_date.day >= 1 && hebrew_date.day <= 30)
  
  // Test Chinese calendar conversion
  let chinese_date = CalendarManager::convert_to_calendar(
    calendar_manager,
    2021,
    1,
    15,
    Gregorian,
    Chinese
  )
  // Should be a valid Chinese calendar date
  assert_true(chinese_date.year > 4700) // Should be in the 4700s
  assert_true(chinese_date.month >= 1 && chinese_date.month <= 12)
  assert_true(chinese_date.day >= 1 && chinese_date.day <= 30)
  
  // Test time zone conversion
  let timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  let utc_time = CalendarManager::convert_to_timezone(calendar_manager, timestamp, "UTC")
  assert_eq(utc_time.year, 2021)
  assert_eq(utc_time.month, 1)
  assert_eq(utc_time.day, 1)
  assert_eq(utc_time.hour, 0)
  assert_eq(utc_time.minute, 0)
  
  let est_time = CalendarManager::convert_to_timezone(calendar_manager, timestamp, "America/New_York")
  // UTC-5 in winter (standard time)
  assert_eq(est_time.year, 2020) // Previous day in EST
  assert_eq(est_time.month, 12)
  assert_eq(est_time.day, 31)
  assert_eq(est_time.hour, 19) // 19:00 previous day
  
  let jst_time = CalendarManager::convert_to_timezone(calendar_manager, timestamp, "Asia/Tokyo")
  // UTC+9
  assert_eq(jst_time.year, 2021)
  assert_eq(jst_time.month, 1)
  assert_eq(jst_time.day, 1)
  assert_eq(jst_time.hour, 9)
  assert_eq(jst_time.minute, 0)
  
  // Test daylight saving time
  let dst_timestamp = 1622505600000L // 2021-06-01 00:00:00 UTC (summer in Northern Hemisphere)
  
  let est_dst_time = CalendarManager::convert_to_timezone(calendar_manager, dst_timestamp, "America/New_York")
  // UTC-4 in summer (daylight saving time)
  assert_eq(est_dst_time.year, 2021)
  assert_eq(est_dst_time.month, 5)
  assert_eq(est_dst_time.day, 31)
  assert_eq(est_dst_time.hour, 20) // 20:00 previous day
}

// Test 8: Internationalized Telemetry Data
test "internationalized telemetry data" {
  // Create an internationalized telemetry processor
  let i18n_processor = I18nTelemetryProcessor::new()
  
  // Set up locales for testing
  I18nTelemetryProcessor::set_locale(i18n_processor, Locale::new("en", "US"))
  
  // Create telemetry data with internationalized values
  let telemetry_data = TelemetryData::new()
  
  // Add metrics with localized names
  I18nTelemetryProcessor::add_localized_metric(
    i18n_processor,
    telemetry_data,
    "cpu.usage",
    "CPU Usage",
    75.5
  )
  
  I18nTelemetryProcessor::add_localized_metric(
    i18n_processor,
    telemetry_data,
    "memory.usage",
    "Memory Usage",
    60.2
  )
  
  // Add events with localized messages
  let event_attrs = Attributes::new()
  Attributes::set(event_attrs, "user_id", StringValue("user-123"))
  
  I18nTelemetryProcessor::add_localized_event(
    i18n_processor,
    telemetry_data,
    "user.login",
    "User Login",
    event_attrs
  )
  
  // Verify data in English
  let en_metrics = TelemetryData::metrics(telemetry_data)
  assert_eq(en_metrics[0].localized_name, "CPU Usage")
  assert_eq(en_metrics[1].localized_name, "Memory Usage")
  
  let en_events = TelemetryData::events(telemetry_data)
  assert_eq(en_events[0].localized_message, "User Login")
  
  // Switch to French locale
  I18nTelemetryProcessor::set_locale(i18n_processor, Locale::new("fr", "FR"))
  
  // Add translations for metric names and event messages
  I18nTelemetryProcessor::add_translation(
    i18n_processor,
    "en",
    "US",
    "cpu.usage",
    "CPU Usage"
  )
  I18nTelemetryProcessor::add_translation(
    i18n_processor,
    "fr",
    "FR",
    "cpu.usage",
    "Utilisation du CPU"
  )
  
  I18nTelemetryProcessor::add_translation(
    i18n_processor,
    "en",
    "US",
    "memory.usage",
    "Memory Usage"
  )
  I18nTelemetryProcessor::add_translation(
    i18n_processor,
    "fr",
    "FR",
    "memory.usage",
    "Utilisation de la Mémoire"
  )
  
  I18nTelemetryProcessor::add_translation(
    i18n_processor,
    "en",
    "US",
    "user.login",
    "User Login"
  )
  I18nTelemetryProcessor::add_translation(
    i18n_processor,
    "fr",
    "FR",
    "user.login",
    "Connexion Utilisateur"
  )
  
  // Create a new telemetry data set in French
  let fr_telemetry_data = TelemetryData::new()
  
  I18nTelemetryProcessor::add_localized_metric(
    i18n_processor,
    fr_telemetry_data,
    "cpu.usage",
    "CPU Usage",
    80.0
  )
  
  I18nTelemetryProcessor::add_localized_metric(
    i18n_processor,
    fr_telemetry_data,
    "memory.usage",
    "Memory Usage",
    70.5
  )
  
  I18nTelemetryProcessor::add_localized_event(
    i18n_processor,
    fr_telemetry_data,
    "user.login",
    "User Login",
    event_attrs
  )
  
  // Verify data in French
  let fr_metrics = TelemetryData::metrics(fr_telemetry_data)
  assert_eq(fr_metrics[0].localized_name, "Utilisation du CPU")
  assert_eq(fr_metrics[1].localized_name, "Utilisation de la Mémoire")
  
  let fr_events = TelemetryData::events(fr_telemetry_data)
  assert_eq(fr_events[0].localized_message, "Connexion Utilisateur")
  
  // Test formatting of metric values according to locale
  let formatted_value = I18nTelemetryProcessor::format_metric_value(
    i18n_processor,
    1234.567,
    Locale::new("fr", "FR")
  )
  assert_eq(formatted_value, "1 234,567") // French number format
  
  // Test serialization with locale information
  let serialized_data = I18nTelemetryProcessor::serialize_with_locale(
    i18n_processor,
    fr_telemetry_data,
    Locale::new("fr", "FR")
  )
  
  assert_true(serialized_data.includes("\"locale\":\"fr-FR\""))
  assert_true(serialized_data.includes("\"localized_name\":\"Utilisation du CPU\""))
  
  // Test deserialization with locale information
  let deserialized_data = I18nTelemetryProcessor::deserialize_with_locale(
    i18n_processor,
    serialized_data
  )
  
  let deserialized_metrics = TelemetryData::metrics(deserialized_data)
  assert_eq(deserialized_metrics[0].localized_name, "Utilisation du CPU")
  assert_eq(deserialized_metrics[0].value, 80.0)
}