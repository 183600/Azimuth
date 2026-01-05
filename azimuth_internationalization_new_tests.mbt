// Azimuth Telemetry System - Internationalization Tests
// This file contains test cases for internationalization and localization functionality

// Test 1: Basic Localization
test "basic localization" {
  let localization_manager = LocalizationManager::new()
  
  // Add localization resources
  LocalizationManager::add_resource(localization_manager, "en", {
    "welcome": "Welcome",
    "goodbye": "Goodbye",
    "error_occurred": "An error occurred: {0}",
    "items_count": "You have {0} items"
  })
  
  LocalizationManager::add_resource(localization_manager, "es", {
    "welcome": "Bienvenido",
    "goodbye": "Adiós",
    "error_occurred": "Ocurrió un error: {0}",
    "items_count": "Tienes {0} elementos"
  })
  
  LocalizationManager::add_resource(localization_manager, "fr", {
    "welcome": "Bienvenue",
    "goodbye": "Au revoir",
    "error_occurred": "Une erreur s'est produite: {0}",
    "items_count": "Vous avez {0} éléments"
  })
  
  // Test English localization
  LocalizationManager::set_locale(localization_manager, "en")
  assert_eq(LocalizationManager::translate(localization_manager, "welcome"), "Welcome")
  assert_eq(LocalizationManager::translate(localization_manager, "goodbye"), "Goodbye")
  
  // Test Spanish localization
  LocalizationManager::set_locale(localization_manager, "es")
  assert_eq(LocalizationManager::translate(localization_manager, "welcome"), "Bienvenido")
  assert_eq(LocalizationManager::translate(localization_manager, "goodbye"), "Adiós")
  
  // Test French localization
  LocalizationManager::set_locale(localization_manager, "fr")
  assert_eq(LocalizationManager::translate(localization_manager, "welcome"), "Bienvenue")
  assert_eq(LocalizationManager::translate(localization_manager, "goodbye"), "Au revoir")
}

// Test 2: Parameterized Localization
test "parameterized localization" {
  let localization_manager = LocalizationManager::new()
  
  // Add parameterized resources
  LocalizationManager::add_resource(localization_manager, "en", {
    "user_greeting": "Hello, {0}!",
    "error_with_code": "Error {0}: {1}",
    "file_not_found": "File '{0}' not found at path '{1}'",
    "items_in_cart": "You have {0} items in your cart"
  })
  
  LocalizationManager::add_resource(localization_manager, "es", {
    "user_greeting": "¡Hola, {0}!",
    "error_with_code": "Error {0}: {1}",
    "file_not_found": "Archivo '{0}' no encontrado en la ruta '{1}'",
    "items_in_cart": "Tienes {0} artículos en tu carrito"
  })
  
  // Test English parameterized strings
  LocalizationManager::set_locale(localization_manager, "en")
  let greeting = LocalizationManager::format(localization_manager, "user_greeting", ["Alice"])
  assert_eq(greeting, "Hello, Alice!")
  
  let error = LocalizationManager::format(localization_manager, "error_with_code", ["404", "Not Found"])
  assert_eq(error, "Error 404: Not Found")
  
  let file_error = LocalizationManager::format(localization_manager, "file_not_found", ["config.txt", "/etc/app/"])
  assert_eq(file_error, "File 'config.txt' not found at path '/etc/app/'")
  
  // Test Spanish parameterized strings
  LocalizationManager::set_locale(localization_manager, "es")
  let spanish_greeting = LocalizationManager::format(localization_manager, "user_greeting", ["Alice"])
  assert_eq(spanish_greeting, "¡Hola, Alice!")
  
  let spanish_error = LocalizationManager::format(localization_manager, "error_with_code", ["404", "No Encontrado"])
  assert_eq(spanish_error, "Error 404: No Encontrado")
}

// Test 3: Pluralization
test "pluralization" {
  let localization_manager = LocalizationManager::new()
  
  // Add pluralization rules
  LocalizationManager::add_plural_resource(localization_manager, "en", "item_count", {
    PluralRule::zero: "No items",
    PluralRule::one: "One item",
    PluralRule::other: "{0} items"
  })
  
  LocalizationManager::add_plural_resource(localization_manager, "es", "item_count", {
    PluralRule::zero: "Sin artículos",
    PluralRule::one: "Un artículo",
    PluralRule::other: "{0} artículos"
  })
  
  LocalizationManager::add_plural_resource(localization_manager, "fr", "item_count", {
    PluralRule::zero: "Aucun article",
    PluralRule::one: "Un article",
    PluralRule::other: "{0} articles"
  })
  
  // Test English pluralization
  LocalizationManager::set_locale(localization_manager, "en")
  assert_eq(LocalizationManager::pluralize(localization_manager, "item_count", 0), "No items")
  assert_eq(LocalizationManager::pluralize(localization_manager, "item_count", 1), "One item")
  assert_eq(LocalizationManager::pluralize(localization_manager, "item_count", 5), "5 items")
  
  // Test Spanish pluralization
  LocalizationManager::set_locale(localization_manager, "es")
  assert_eq(LocalizationManager::pluralize(localization_manager, "item_count", 0), "Sin artículos")
  assert_eq(LocalizationManager::pluralize(localization_manager, "item_count", 1), "Un artículo")
  assert_eq(LocalizationManager::pluralize(localization_manager, "item_count", 5), "5 artículos")
  
  // Test French pluralization
  LocalizationManager::set_locale(localization_manager, "fr")
  assert_eq(LocalizationManager::pluralize(localization_manager, "item_count", 0), "Aucun article")
  assert_eq(LocalizationManager::pluralize(localization_manager, "item_count", 1), "Un article")
  assert_eq(LocalizationManager::pluralize(localization_manager, "item_count", 5), "5 articles")
}

// Test 4: Date and Time Formatting
test "date and time formatting" {
  let localization_manager = LocalizationManager::new()
  
  // Add locale-specific date formats
  LocalizationManager::add_date_format(localization_manager, "en", "MM/DD/YYYY")
  LocalizationManager::add_date_format(localization_manager, "en", "short", "M/D/YY")
  LocalizationManager::add_date_format(localization_manager, "en", "long", "MMMM D, YYYY")
  
  LocalizationManager::add_date_format(localization_manager, "es", "DD/MM/YYYY")
  LocalizationManager::add_date_format(localization_manager, "es", "short", "D/M/YY")
  LocalizationManager::add_date_format(localization_manager, "es", "long", "D [de] MMMM [de] YYYY")
  
  LocalizationManager::add_date_format(localization_manager, "fr", "DD/MM/YYYY")
  LocalizationManager::add_date_format(localization_manager, "fr", "short", "D/M/YY")
  LocalizationManager::add_date_format(localization_manager, "fr", "long", "D MMMM YYYY")
  
  // Create test date (January 15, 2023)
  let test_date = DateTime::new(2023, 1, 15, 14, 30, 0)
  
  // Test English date formatting
  LocalizationManager::set_locale(localization_manager, "en")
  let en_default = LocalizationManager::format_date(localization_manager, test_date)
  let en_short = LocalizationManager::format_date_with_format(localization_manager, test_date, "short")
  let en_long = LocalizationManager::format_date_with_format(localization_manager, test_date, "long")
  
  assert_eq(en_default, "01/15/2023")
  assert_eq(en_short, "1/15/23")
  assert_eq(en_long, "January 15, 2023")
  
  // Test Spanish date formatting
  LocalizationManager::set_locale(localization_manager, "es")
  let es_default = LocalizationManager::format_date(localization_manager, test_date)
  let es_short = LocalizationManager::format_date_with_format(localization_manager, test_date, "short")
  let es_long = LocalizationManager::format_date_with_format(localization_manager, test_date, "long")
  
  assert_eq(es_default, "15/01/2023")
  assert_eq(es_short, "15/1/23")
  assert_eq(es_long, "15 de enero de 2023")
  
  // Test French date formatting
  LocalizationManager::set_locale(localization_manager, "fr")
  let fr_default = LocalizationManager::format_date(localization_manager, test_date)
  let fr_short = LocalizationManager::format_date_with_format(localization_manager, test_date, "short")
  let fr_long = LocalizationManager::format_date_with_format(localization_manager, test_date, "long")
  
  assert_eq(fr_default, "15/01/2023")
  assert_eq(fr_short, "15/1/23")
  assert_eq(fr_long, "15 janvier 2023")
}

// Test 5: Number Formatting
test "number formatting" {
  let localization_manager = LocalizationManager::new()
  
  // Add locale-specific number formats
  LocalizationManager::add_number_format(localization_manager, "en", {
    "decimal_separator": ".",
    "thousands_separator": ",",
    "currency_symbol": "$",
    "currency_position": "before"
  })
  
  LocalizationManager::add_number_format(localization_manager, "es", {
    "decimal_separator": ",",
    "thousands_separator": ".",
    "currency_symbol": "€",
    "currency_position": "after"
  })
  
  LocalizationManager::add_number_format(localization_manager, "fr", {
    "decimal_separator": ",",
    "thousands_separator": " ",
    "currency_symbol": "€",
    "currency_position": "after"
  })
  
  // Test English number formatting
  LocalizationManager::set_locale(localization_manager, "en")
  let en_decimal = LocalizationManager::format_number(localization_manager, 1234.56)
  let en_currency = LocalizationManager::format_currency(localization_manager, 1234.56)
  
  assert_eq(en_decimal, "1,234.56")
  assert_eq(en_currency, "$1,234.56")
  
  // Test Spanish number formatting
  LocalizationManager::set_locale(localization_manager, "es")
  let es_decimal = LocalizationManager::format_number(localization_manager, 1234.56)
  let es_currency = LocalizationManager::format_currency(localization_manager, 1234.56)
  
  assert_eq(es_decimal, "1.234,56")
  assert_eq(es_currency, "1.234,56€")
  
  // Test French number formatting
  LocalizationManager::set_locale(localization_manager, "fr")
  let fr_decimal = LocalizationManager::format_number(localization_manager, 1234.56)
  let fr_currency = LocalizationManager::format_currency(localization_manager, 1234.56)
  
  assert_eq(fr_decimal, "1 234,56")
  assert_eq(fr_currency, "1 234,56€")
}

// Test 6: Right-to-Left Language Support
test "right-to-left language support" {
  let localization_manager = LocalizationManager::new()
  
  // Add RTL language resources
  LocalizationManager::add_resource(localization_manager, "ar", {
    "welcome": "مرحبا",
    "goodbye": "وداعا",
    "user_greeting": "مرحبا، {0}!",
    "navigation": {
      "home": "الصفحة الرئيسية",
      "settings": "الإعدادات",
      "profile": "الملف الشخصي"
    }
  })
  
  LocalizationManager::add_resource(localization_manager, "he", {
    "welcome": "שלום",
    "goodbye": "להתראות",
    "user_greeting": "שלום, {0}!",
    "navigation": {
      "home": "דף הבית",
      "settings": "הגדרות",
      "profile": "פרופיל"
    }
  })
  
  // Test Arabic localization
  LocalizationManager::set_locale(localization_manager, "ar")
  assert_true(LocalizationManager::is_rtl(localization_manager))
  assert_eq(LocalizationManager::translate(localization_manager, "welcome"), "مرحبا")
  
  let ar_greeting = LocalizationManager::format(localization_manager, "user_greeting", ["Alice"])
  assert_eq(ar_greeting, "مرحبا، Alice!")
  
  // Test Hebrew localization
  LocalizationManager::set_locale(localization_manager, "he")
  assert_true(LocalizationManager::is_rtl(localization_manager))
  assert_eq(LocalizationManager::translate(localization_manager, "welcome"), "שלום")
  
  let he_greeting = LocalizationManager::format(localization_manager, "user_greeting", ["Alice"])
  assert_eq(he_greeting, "שלום, Alice!")
  
  // Test LTR language for comparison
  LocalizationManager::set_locale(localization_manager, "en")
  assert_false(LocalizationManager::is_rtl(localization_manager))
}

// Test 7: Locale Fallback
test "locale fallback" {
  let localization_manager = LocalizationManager::new()
  
  // Add resources for different locales
  LocalizationManager::add_resource(localization_manager, "en-US", {
    "color": "Color",
    "favorite": "Favorite"
  })
  
  LocalizationManager::add_resource(localization_manager, "en", {
    "color": "Colour",
    "common": "Common"
  })
  
  LocalizationManager::add_resource(localization_manager, "es-ES", {
    "color": "Color",
    "común": "Común"
  })
  
  LocalizationManager::add_resource(localization_manager, "es", {
    "color": "Color",
    "común": "Común"
  })
  
  // Set up fallback chain
  LocalizationManager::set_fallback_chain(localization_manager, ["en-US", "en"])
  
  // Test exact match
  LocalizationManager::set_locale(localization_manager, "en-US")
  assert_eq(LocalizationManager::translate(localization_manager, "color"), "Color")
  assert_eq(LocalizationManager::translate(localization_manager, "favorite"), "Favorite")
  
  // Test fallback to parent locale
  LocalizationManager::set_locale(localization_manager, "en-GB")
  assert_eq(LocalizationManager::translate(localization_manager, "color"), "Colour")
  assert_eq(LocalizationManager::translate(localization_manager, "common"), "Common")
  
  // Test missing key with fallback
  LocalizationManager::set_locale(localization_manager, "en-AU")
  assert_eq(LocalizationManager::translate(localization_manager, "color"), "Colour")
  assert_eq(LocalizationManager::translate(localization_manager, "common"), "Common")
  
  // Test completely missing locale
  LocalizationManager::set_locale(localization_manager, "fr-FR")
  assert_eq(LocalizationManager::translate(localization_manager, "color"), "Colour") // Falls back to "en"
  assert_eq(LocalizationManager::translate(localization_manager, "common"), "Common")
}

// Test 8: Dynamic Locale Loading
test "dynamic locale loading" {
  let localization_manager = LocalizationManager::new()
  
  // Add base locale
  LocalizationManager::add_resource(localization_manager, "en", {
    "welcome": "Welcome",
    "goodbye": "Goodbye"
  })
  
  // Set initial locale
  LocalizationManager::set_locale(localization_manager, "en")
  assert_eq(LocalizationManager::translate(localization_manager, "welcome"), "Welcome")
  
  // Simulate dynamic loading of additional locale
  let spanish_resources = {
    "welcome": "Bienvenido",
    "goodbye": "Adiós",
    "thanks": "Gracias"
  }
  
  LocalizationManager::add_resource(localization_manager, "es", spanish_resources)
  
  // Switch to dynamically loaded locale
  LocalizationManager::set_locale(localization_manager, "es")
  assert_eq(LocalizationManager::translate(localization_manager, "welcome"), "Bienvenido")
  assert_eq(LocalizationManager::translate(localization_manager, "goodbye"), "Adiós")
  assert_eq(LocalizationManager::translate(localization_manager, "thanks"), "Gracias")
  
  // Test updating existing locale
  let updated_spanish_resources = {
    "welcome": "¡Bienvenido!",
    "goodbye": "¡Adiós!",
    "thanks": "¡Gracias!"
  }
  
  LocalizationManager::add_resource(localization_manager, "es", updated_spanish_resources)
  assert_eq(LocalizationManager::translate(localization_manager, "welcome"), "¡Bienvenido!")
  assert_eq(LocalizationManager::translate(localization_manager, "goodbye"), "¡Adiós!")
  assert_eq(LocalizationManager::translate(localization_manager, "thanks"), "¡Gracias!")
}

// Test 9: Context-Aware Localization
test "context-aware localization" {
  let localization_manager = LocalizationManager::new()
  
  // Add context-specific resources
  LocalizationManager::add_context_resource(localization_manager, "en", "formal", {
    "greeting": "Good day",
    "question": "May I assist you?",
    "farewell": "Have a pleasant day"
  })
  
  LocalizationManager::add_context_resource(localization_manager, "en", "informal", {
    "greeting": "Hi there",
    "question": "Need help?",
    "farewell": "See you later"
  })
  
  LocalizationManager::add_context_resource(localization_manager, "es", "formal", {
    "greeting": "Buenos días",
    "question": "¿Puedo ayudarle?",
    "farewell": "Que tenga un buen día"
  })
  
  LocalizationManager::add_context_resource(localization_manager, "es", "informal", {
    "greeting": "Hola",
    "question": "¿Necesitas ayuda?",
    "farewell": "Hasta luego"
  })
  
  // Test formal context
  LocalizationManager::set_locale(localization_manager, "en")
  LocalizationManager::set_context(localization_manager, "formal")
  
  assert_eq(LocalizationManager::translate(localization_manager, "greeting"), "Good day")
  assert_eq(LocalizationManager::translate(localization_manager, "question"), "May I assist you?")
  assert_eq(LocalizationManager::translate(localization_manager, "farewell"), "Have a pleasant day")
  
  // Switch to informal context
  LocalizationManager::set_context(localization_manager, "informal")
  
  assert_eq(LocalizationManager::translate(localization_manager, "greeting"), "Hi there")
  assert_eq(LocalizationManager::translate(localization_manager, "question"), "Need help?")
  assert_eq(LocalizationManager::translate(localization_manager, "farewell"), "See you later")
  
  // Test Spanish with formal context
  LocalizationManager::set_locale(localization_manager, "es")
  LocalizationManager::set_context(localization_manager, "formal")
  
  assert_eq(LocalizationManager::translate(localization_manager, "greeting"), "Buenos días")
  assert_eq(LocalizationManager::translate(localization_manager, "question"), "¿Puedo ayudarle?")
  assert_eq(LocalizationManager::translate(localization_manager, "farewell"), "Que tenga un buen día")
  
  // Test Spanish with informal context
  LocalizationManager::set_context(localization_manager, "informal")
  
  assert_eq(LocalizationManager::translate(localization_manager, "greeting"), "Hola")
  assert_eq(LocalizationManager::translate(localization_manager, "question"), "¿Necesitas ayuda?")
  assert_eq(LocalizationManager::translate(localization_manager, "farewell"), "Hasta luego")
}

// Test 10: Localization Validation
test "localization validation" {
  let localization_manager = LocalizationManager::new()
  
  // Add resources for validation
  LocalizationManager::add_resource(localization_manager, "en", {
    "key1": "Value 1",
    "key2": "Value 2",
    "param1": "Parameter {0}",
    "param2": "Parameters {0} and {1}"
  })
  
  LocalizationManager::add_resource(localization_manager, "es", {
    "key1": "Valor 1",
    "param1": "Parámetro {0}",
    "param2": "Parámetros {0} y {1}"
  })
  
  LocalizationManager::add_resource(localization_manager, "fr", {
    "key1": "Valeur 1",
    "key2": "Valeur 2",
    "param1": "Paramètre {0}"
  })
  
  // Validate translation completeness
  let validation_result = LocalizationManager::validate_completeness(localization_manager)
  
  // Check for missing keys
  assert_true(validation_result.missing_keys.contains(("es", "key2")))
  assert_true(validation_result.missing_keys.contains(("fr", "param2")))
  
  // Check for extra keys
  assert_true(validation_result.extra_keys.contains(("en", "key2")))
  
  // Validate parameter consistency
  let param_validation = LocalizationManager::validate_parameters(localization_manager)
  
  // Check for parameter mismatches
  assert_true(param_validation.parameter_mismatches.contains(("fr", "param2"))) // Missing parameter
  
  // Validate resource integrity
  let integrity_result = LocalizationManager::validate_integrity(localization_manager)
  
  // Check for circular references
  assert_false(integrity_result.has_circular_references)
  
  // Check for malformed parameter placeholders
  assert_false(integrity_result.has_malformed_placeholders)
  
  // Check for encoding issues
  assert_false(integrity_result.has_encoding_issues)
}