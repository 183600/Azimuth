// Azimuth Premium Test Suite - Internationalization and Localization Support
// This file contains comprehensive test cases for internationalization, localization, and multi-language support

// Test 1: Unicode and Character Encoding
test "unicode and character encoding" {
  // Test basic Unicode character handling
  let unicode_string = "Hello, 世界! 🌍"
  
  // Test character counting
  assert_eq(unicode_string.length(), 14)  // Counting code points
  
  // Test character access
  assert_eq(unicode_string[0], "H")
  assert_eq(unicode_string[7], "世")
  assert_eq(unicode_string[8], "界")
  assert_eq(unicode_string[10], "🌍")
  
  // Test Unicode string slicing
  let sliced = unicode_string.slice(7, 9)
  assert_eq(sliced, "世界")
  
  let emoji_slice = unicode_string.slice(10, 11)
  assert_eq(emoji_slice, "🌍")
  
  // Test combining characters
  let combining_string = "café"  // Using precomposed characters
  let decomposed_string = "cafe\u{0301}"  // 'e' + combining acute accent
  
  // Test Unicode normalization
  assert_eq(combining_string.normalize(), decomposed_string.normalize())
  
  // Test various Unicode scripts
  let latin_text = "Hello World"
  let cyrillic_text = "Привет мир"
  let arabic_text = "مرحبا بالعالم"
  let chinese_text = "你好世界"
  let japanese_text = "こんにちは世界"
  let korean_text = "안녕하세요 세계"
  let hebrew_text = "שלום עולם"
  let hindi_text = "नमस्ते दुनिया"
  let thai_text = "สวัสดีชาวโลก"
  
  // Test character properties
  assert_true(latin_text[0].is_latin())
  assert_true(cyrillic_text[0].is_cyrillic())
  assert_true(arabic_text[0].is_arabic())
  assert_true(chinese_text[0].is_chinese())
  assert_true(japanese_text[0].is_japanese())
  assert_true(korean_text[0].is_korean())
  assert_true(hebrew_text[0].is_hebrew())
  assert_true(hindi_text[0].is_devanagari())
  assert_true(thai_text[0].is_thai())
  
  // Test bidirectional text
  let mixed_ltr_rtl = "Hello مرحبا World"
  assert_true(mixed_ltr_rtl.contains_latin())
  assert_true(mixed_ltr_rtl.contains_arabic())
  
  let rtl_text = "مرحبا بالعالم"
  assert_true(rtl_text.is_right_to_left())
  
  let ltr_text = "Hello World"
  assert_true(ltr_text.is_left_to_right())
  
  // Test Unicode case conversion
  let mixed_case = "HeLLo, 世界! hElLo"
  let uppercase = mixed_case.to_uppercase()
  let lowercase = mixed_case.to_lowercase()
  
  assert_true(uppercase.contains("HELLO"))
  assert_true(uppercase.contains("世界"))
  assert_true(lowercase.contains("hello"))
  assert_true(lowercase.contains("世界"))
  
  // Test Unicode character categories
  assert_true("A".is_uppercase())
  assert_true("a".is_lowercase())
  assert_true("1".is_digit())
  assert_true(" ".is_whitespace())
  assert_true("!".is_punctuation())
  
  // Test surrogate pairs (for characters outside BMP)
  let surrogate_char = "𝄞"  // Musical symbol G clef
  assert_eq(surrogate_char.length(), 1)  // Should be treated as single character
  assert_true(surrogate_char.is_surrogate_pair())
}

// Test 2: Locale-Aware Formatting and Parsing
test "locale_aware_formatting_and_parsing" {
  // Define locale configurations
  let locales = {
    "en-US": {
      name: "English (United States)",
      decimal_separator: ".",
      thousands_separator: ",",
      date_format: "MM/DD/YYYY",
      time_format: "h:mm:ss A",
      currency_symbol: "$",
      currency_position: "before"
    },
    "fr-FR": {
      name: "French (France)",
      decimal_separator: ",",
      thousands_separator: " ",
      date_format: "DD/MM/YYYY",
      time_format: "HH:mm:ss",
      currency_symbol: "€",
      currency_position: "after"
    },
    "de-DE": {
      name: "German (Germany)",
      decimal_separator: ",",
      thousands_separator: ".",
      date_format: "DD.MM.YYYY",
      time_format: "HH:mm:ss",
      currency_symbol: "€",
      currency_position: "after"
    },
    "ja-JP": {
      name: "Japanese (Japan)",
      decimal_separator: ".",
      thousands_separator: ",",
      date_format: "YYYY/MM/DD",
      time_format: "HH:mm:ss",
      currency_symbol: "¥",
      currency_position: "before"
    },
    "ar-SA": {
      name: "Arabic (Saudi Arabia)",
      decimal_separator: "٫",  // Arabic decimal separator
      thousands_separator: "٬",  // Arabic thousands separator
      date_format: "DD/MM/YYYY",
      time_format: "HH:mm:ss",
      currency_symbol: "ر.س",
      currency_position: "before"
    }
  }
  
  // Test number formatting
  let format_number = |number, locale_code| {
    let locale = locales[locale_code]
    let number_str = number.to_string()
    
    // Simple formatting simulation
    if locale_code == "en-US" {
      if number >= 1000 {
        let thousands = number / 1000
        let remainder = number % 1000
        thousands.to_string() + locale.thousands_separator + remainder.to_string()
      } else {
        number_str
      }
    } else if locale_code == "fr-FR" {
      if number >= 1000 {
        let thousands = number / 1000
        let remainder = number % 1000
        thousands.to_string() + locale.thousands_separator + remainder.to_string()
      } else {
        number_str
      }
    } else {
      number_str
    }
  }
  
  // Test number formatting with different locales
  assert_eq(format_number(1234.56, "en-US"), "1234.56")
  assert_eq(format_number(1234567, "en-US"), "1234,567")
  assert_eq(format_number(1234567, "fr-FR"), "1234 567")
  
  // Test currency formatting
  let format_currency = |amount, locale_code| {
    let locale = locales[locale_code]
    let amount_str = amount.to_string()
    
    if locale.currency_position == "before" {
      locale.currency_symbol + amount_str
    } else {
      amount_str + " " + locale.currency_symbol
    }
  }
  
  assert_eq(format_currency(123.45, "en-US"), "$123.45")
  assert_eq(format_currency(123.45, "fr-FR"), "123.45 €")
  assert_eq(format_currency(123.45, "ja-JP"), "¥123.45")
  assert_eq(format_currency(123.45, "ar-SA"), "ر.س123.45")
  
  // Test date formatting
  let format_date = |year, month, day, locale_code| {
    let locale = locales[locale_code]
    
    if locale_code == "en-US" {
      month.to_string() + "/" + day.to_string() + "/" + year.to_string()
    } else if locale_code == "fr-FR" || locale_code == "de-DE" {
      day.to_string() + "/" + month.to_string() + "/" + year.to_string()
    } else if locale_code == "ja-JP" {
      year.to_string() + "/" + month.to_string() + "/" + day.to_string()
    } else {
      year.to_string() + "-" + month.to_string() + "-" + day.to_string()
    }
  }
  
  assert_eq(format_date(2023, 12, 25, "en-US"), "12/25/2023")
  assert_eq(format_date(2023, 12, 25, "fr-FR"), "25/12/2023")
  assert_eq(format_date(2023, 12, 25, "de-DE"), "25/12/2023")
  assert_eq(format_date(2023, 12, 25, "ja-JP"), "2023/12/25")
  
  // Test time formatting
  let format_time = |hour, minute, second, locale_code| {
    let locale = locales[locale_code]
    
    if locale_code == "en-US" {
      let period = if hour >= 12 { "PM" } else { "AM" }
      let display_hour = if hour > 12 { hour - 12 } else if hour == 0 { 12 } else { hour }
      display_hour.to_string() + ":" + minute.to_string() + ":" + second.to_string() + " " + period
    } else {
      hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
    }
  }
  
  assert_eq(format_time(14, 30, 45, "en-US"), "2:30:45 PM")
  assert_eq(format_time(14, 30, 45, "fr-FR"), "14:30:45")
  assert_eq(format_time(9, 15, 30, "en-US"), "9:15:30 AM")
  
  // Test list formatting
  let format_list = |items, locale_code| {
    if locale_code == "en-US" {
      if items.length() == 1 {
        items[0]
      } else if items.length() == 2 {
        items[0] + " and " + items[1]
      } else {
        let mut result = ""
        for i = 0; i < items.length(); i = i + 1 {
          if i == items.length() - 1 {
            result = result + "and " + items[i]
          } else if i == items.length() - 2 {
            result = result + items[i] + " "
          } else {
            result = result + items[i] + ", "
          }
        }
        result
      }
    } else if locale_code == "fr-FR" {
      if items.length() == 1 {
        items[0]
      } else if items.length() == 2 {
        items[0] + " et " + items[1]
      } else {
        let mut result = ""
        for i = 0; i < items.length(); i = i + 1 {
          if i == items.length() - 1 {
            result = result + "et " + items[i]
          } else if i == items.length() - 2 {
            result = result + items[i] + " "
          } else {
            result = result + items[i] + ", "
          }
        }
        result
      }
    } else {
      items.join(", ")
    }
  }
  
  let fruits = ["apple", "banana", "orange"]
  assert_eq(format_list(fruits, "en-US"), "apple, banana and orange")
  assert_eq(format_list(fruits, "fr-FR"), "apple, banana et orange")
  
  let two_items = ["cat", "dog"]
  assert_eq(format_list(two_items, "en-US"), "cat and dog")
  assert_eq(format_list(two_items, "fr-FR"), "cat et dog")
}

// Test 3: Resource Bundle Management
test "resource_bundle_management" {
  // Define resource bundles for different locales
  let resource_bundles = {
    "en": {
      "greeting.hello": "Hello",
      "greeting.goodbye": "Goodbye",
      "error.not_found": "Resource not found",
      "error.permission_denied": "Permission denied",
      "button.save": "Save",
      "button.cancel": "Cancel",
      "menu.file": "File",
      "menu.edit": "Edit",
      "menu.view": "View",
      "message.welcome": "Welcome to our application!",
      "confirmation.delete": "Are you sure you want to delete this item?"
    },
    "fr": {
      "greeting.hello": "Bonjour",
      "greeting.goodbye": "Au revoir",
      "error.not_found": "Ressource non trouvée",
      "error.permission_denied": "Permission refusée",
      "button.save": "Enregistrer",
      "button.cancel": "Annuler",
      "menu.file": "Fichier",
      "menu.edit": "Editer",
      "menu.view": "Affichage",
      "message.welcome": "Bienvenue dans notre application!",
      "confirmation.delete": "Êtes-vous sûr de vouloir supprimer cet élément?"
    },
    "es": {
      "greeting.hello": "Hola",
      "greeting.goodbye": "Adiós",
      "error.not_found": "Recurso no encontrado",
      "error.permission_denied": "Permiso denegado",
      "button.save": "Guardar",
      "button.cancel": "Cancelar",
      "menu.file": "Archivo",
      "menu.edit": "Editar",
      "menu.view": "Ver",
      "message.welcome": "¡Bienvenido a nuestra aplicación!",
      "confirmation.delete": "¿Estás seguro de que quieres eliminar este elemento?"
    },
    "de": {
      "greeting.hello": "Hallo",
      "greeting.goodbye": "Auf Wiedersehen",
      "error.not_found": "Ressource nicht gefunden",
      "error.permission_denied": "Zugriff verweigert",
      "button.save": "Speichern",
      "button.cancel": "Abbrechen",
      "menu.file": "Datei",
      "menu.edit": "Bearbeiten",
      "menu.view": "Ansicht",
      "message.welcome": "Willkommen in unserer Anwendung!",
      "confirmation.delete": "Möchten Sie dieses Element wirklich löschen?"
    },
    "ja": {
      "greeting.hello": "こんにちは",
      "greeting.goodbye": "さようなら",
      "error.not_found": "リソースが見つかりません",
      "error.permission_denied": "アクセス拒否",
      "button.save": "保存",
      "button.cancel": "キャンセル",
      "menu.file": "ファイル",
      "menu.edit": "編集",
      "menu.view": "表示",
      "message.welcome": "アプリケーションへようこそ！",
      "confirmation.delete": "このアイテムを削除してもよろしいですか？"
    }
  }
  
  // Resource bundle manager
  let resource_bundle_manager = |bundles| {
    let mut current_locale = "en"
    
    let set_locale = |locale| {
      if bundles.contains(locale) {
        current_locale = locale
        true
      } else {
        false
      }
    }
    
    let get_string = |key| {
      if bundles.contains(current_locale) && bundles[current_locale].contains(key) {
        Some(bundles[current_locale][key])
      } else if bundles.contains("en") && bundles["en"].contains(key) {
        // Fallback to English
        Some(bundles["en"][key])
      } else {
        None
      }
    }
    
    let get_string_with_args = |key, args| {
      match get_string(key) {
        Some(template) => {
          let mut result = template
          for i = 0; i < args.length(); i = i + 1 {
            result = result.replace("{" + i.to_string() + "}", args[i])
          }
          result
        }
        None => key
      }
    }
    
    let get_current_locale = || {
      current_locale
    }
    
    let get_available_locales = || {
      let mut locales = []
      for locale in bundles {
        locales.push(locale)
      }
      locales
    }
    
    (set_locale, get_string, get_string_with_args, get_current_locale, get_available_locales)
  }
  
  let (set_locale, get_string, get_string_with_args, get_current_locale, get_available_locales) = resource_bundle_manager(resource_bundles)
  
  // Test locale setting
  assert_true(set_locale("fr"))
  assert_eq(get_current_locale(), "fr")
  
  assert_false(set_locale("invalid"))  // Invalid locale
  assert_eq(get_current_locale(), "fr")  // Should remain unchanged
  
  // Test string retrieval
  assert_eq(get_string("greeting.hello"), Some("Bonjour"))
  assert_eq(get_string("greeting.goodbye"), Some("Au revoir"))
  assert_eq(get_string("button.save"), Some("Enregistrer"))
  
  // Test fallback to English
  set_locale("invalid_locale")  // This will fail but keep current locale
  set_locale("zh")  // This will fail but keep current locale
  
  // Test with non-existent key
  assert_eq(get_string("nonexistent.key"), None)
  
  // Test string formatting with arguments
  set_locale("en")
  let formatted = get_string_with_args("confirmation.delete", ["this item"])
  assert_eq(formatted, "Are you sure you want to delete this item?")
  
  // Test different locales
  set_locale("es")
  assert_eq(get_string("greeting.hello"), Some("Hola"))
  assert_eq(get_string("button.save"), Some("Guardar"))
  
  set_locale("de")
  assert_eq(get_string("greeting.hello"), Some("Hallo"))
  assert_eq(get_string("button.cancel"), Some("Abbrechen"))
  
  set_locale("ja")
  assert_eq(get_string("greeting.hello"), Some("こんにちは"))
  assert_eq(get_string("menu.file"), Some("ファイル"))
  
  // Test available locales
  let available = get_available_locales()
  assert_eq(available.length(), 5)
  assert_true(available.contains("en"))
  assert_true(available.contains("fr"))
  assert_true(available.contains("es"))
  assert_true(available.contains("de"))
  assert_true(available.contains("ja"))
  
  // Test hierarchical resource bundles
  let hierarchical_bundles = {
    "en-US": {
      "greeting.hello": "Howdy!",
      "error.not_found": "Resource not found"
    },
    "en": {
      "greeting.hello": "Hello",
      "greeting.goodbye": "Goodbye",
      "error.not_found": "Resource not found",
      "error.permission_denied": "Permission denied"
    }
  }
  
  let (set_locale_h, get_string_h, _, _, _) = resource_bundle_manager(hierarchical_bundles)
  
  // Test locale fallback (en-US -> en)
  set_locale_h("en-US")
  assert_eq(get_string_h("greeting.hello"), Some("Howdy!"))  // From en-US
  assert_eq(get_string_h("greeting.goodbye"), Some("Goodbye"))  // From en
  assert_eq(get_string_h("error.permission_denied"), Some("Permission denied"))  // From en
}

// Test 4: Text Direction and Bidirectional Support
test "text_direction_and_bidirectional_support" {
  // Test text direction detection
  let detect_text_direction = |text| {
    let mut ltr_chars = 0
    let mut rtl_chars = 0
    
    for i = 0; i < text.length(); i = i + 1 {
      let char = text[i]
      if char.is_arabic() || char.is_hebrew() {
        rtl_chars = rtl_chars + 1
      } else if char.is_latin() {
        ltr_chars = ltr_chars + 1
      }
    }
    
    if rtl_chars > ltr_chars {
      "rtl"
    } else if ltr_chars > rtl_chars {
      "ltr"
    } else {
      "neutral"
    }
  }
  
  // Test text direction detection
  assert_eq(detect_text_direction("Hello World"), "ltr")
  assert_eq(detect_text_direction("مرحبا بالعالم"), "rtl")
  assert_eq(detect_text_direction("שלום עולם"), "rtl")
  assert_eq(detect_text_direction("12345"), "neutral")
  
  // Test mixed direction text
  let mixed_text = "Hello مرحبا World"
  assert_eq(detect_text_direction(mixed_text), "ltr")  // More LTR characters
  
  let mixed_text2 = "مرحبا Hello مرحبا"
  assert_eq(detect_text_direction(mixed_text2), "rtl")  // More RTL characters
  
  // Test bidirectional text processing
  let process_bidi_text = |text, base_direction| {
    let mut segments = []
    let mut current_segment = ""
    let mut current_direction = base_direction
    
    for i = 0; i < text.length(); i = i + 1 {
      let char = text[i]
      let char_direction = if char.is_arabic() || char.is_hebrew() {
        "rtl"
      } else {
        "ltr"
      }
      
      if char_direction != current_direction {
        if current_segment.length() > 0 {
          segments.push({
            text: current_segment,
            direction: current_direction
          })
        }
        current_segment = char
        current_direction = char_direction
      } else {
        current_segment = current_segment + char
      }
    }
    
    if current_segment.length() > 0 {
      segments.push({
        text: current_segment,
        direction: current_direction
      })
    }
    
    segments
  }
  
  // Test bidirectional text processing
  let bidi_segments = process_bidi_text("Hello مرحبا World", "ltr")
  assert_eq(bidi_segments.length(), 3)
  assert_eq(bidi_segments[0].text, "Hello ")
  assert_eq(bidi_segments[0].direction, "ltr")
  assert_eq(bidi_segments[1].text, "مرحبا ")
  assert_eq(bidi_segments[1].direction, "rtl")
  assert_eq(bidi_segments[2].text, "World")
  assert_eq(bidi_segments[2].direction, "ltr")
  
  let bidi_segments2 = process_bidi_text("مرحبا Hello مرحبا", "rtl")
  assert_eq(bidi_segments2.length(), 3)
  assert_eq(bidi_segments2[0].text, "مرحبا ")
  assert_eq(bidi_segments2[0].direction, "rtl")
  assert_eq(bidi_segments2[1].text, "Hello ")
  assert_eq(bidi_segments2[1].direction, "ltr")
  assert_eq(bidi_segments2[2].text, "مرحبا")
  assert_eq(bidi_segments2[2].direction, "rtl")
  
  // Test text alignment based on direction
  let get_text_alignment = |text, locale| {
    let direction = detect_text_direction(text)
    
    if direction == "rtl" {
      "right"
    } else if direction == "ltr" {
      "left"
    } else {
      // Use locale-specific alignment
      if locale == "ar" || locale == "he" {
        "right"
      } else {
        "left"
      }
    }
  }
  
  assert_eq(get_text_alignment("Hello World", "en"), "left")
  assert_eq(get_text_alignment("مرحبا بالعالم", "ar"), "right")
  assert_eq(get_text_alignment("12345", "en"), "left")
  assert_eq(get_text_alignment("12345", "ar"), "right")
  
  // Test layout mirroring
  let should_mirror_layout = |locale| {
    let rtl_locales = ["ar", "he", "fa", "ur"]
    rtl_locales.contains(locale)
  }
  
  assert_true(should_mirror_layout("ar"))
  assert_true(should_mirror_layout("he"))
  assert_false(should_mirror_layout("en"))
  assert_false(should_mirror_layout("fr"))
  assert_false(should_mirror_layout("ja"))
  
  // Test margin/padding adjustments for RTL
  let adjust_margin_for_rtl = |margin, is_rtl| {
    if is_rtl {
      // Swap left and right margins
      {
        top: margin.top,
        right: margin.left,
        bottom: margin.bottom,
        left: margin.right
      }
    } else {
      margin
    }
  }
  
  let original_margin = { top: 10, right: 20, bottom: 10, left: 15 }
  let adjusted_margin = adjust_margin_for_rtl(original_margin, true)
  
  assert_eq(adjusted_margin.top, 10)
  assert_eq(adjusted_margin.right, 15)  // Swapped with left
  assert_eq(adjusted_margin.bottom, 10)
  assert_eq(adjusted_margin.left, 20)   // Swapped with right
  
  // Test text ordering in mixed content
  let reorder_mixed_text = |segments, base_direction| {
    if base_direction == "rtl" {
      // Reverse order for RTL base direction
      let mut reversed = []
      for i = segments.length() - 1; i >= 0; i = i - 1 {
        reversed.push(segments[i])
      }
      reversed
    } else {
      segments
    }
  }
  
  let mixed_segments = [
    { text: "Start ", direction: "ltr" },
    { text: "وسط", direction: "rtl" },
    { text: " End", direction: "ltr" }
  ]
  
  let ltr_ordered = reorder_mixed_text(mixed_segments, "ltr")
  assert_eq(ltr_ordered.length(), 3)
  assert_eq(ltr_ordered[0].text, "Start ")
  assert_eq(ltr_ordered[1].text, "وسط")
  assert_eq(ltr_ordered[2].text, " End")
  
  let rtl_ordered = reorder_mixed_text(mixed_segments, "rtl")
  assert_eq(rtl_ordered.length(), 3)
  assert_eq(rtl_ordered[0].text, " End")
  assert_eq(rtl_ordered[1].text, "وسط")
  assert_eq(rtl_ordered[2].text, "Start ")
}

// Test 5: Calendar and Date/Time Localization
test "calendar_and_date_time_localization" {
  // Define calendar systems
  let calendar_systems = {
    "gregorian": {
      name: "Gregorian Calendar",
      months: ["January", "February", "March", "April", "May", "June", 
                "July", "August", "September", "October", "November", "December"],
      days: ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"],
      first_day_of_week: 0  // Sunday
    },
    "islamic": {
      name: "Islamic Calendar",
      months: ["Muharram", "Safar", "Rabi' al-awwal", "Rabi' al-thani", "Jumada al-awwal", "Jumada al-thani",
                "Rajab", "Sha'ban", "Ramadan", "Shawwal", "Dhu al-Qi'dah", "Dhu al-Hijjah"],
      days: ["Al-Ahad", "Al-Ithnayn", "Al-Thalatha", "Al-Arba'a", "Al-Khamis", "Al-Jum'a", "Al-Sabt"],
      first_day_of_week: 6  // Saturday
    },
    "hebrew": {
      name: "Hebrew Calendar",
      months: ["Tishrei", "Cheshvan", "Kislev", "Tevet", "Shevat", "Adar", "Nisan",
                "Iyar", "Sivan", "Tammuz", "Av", "Elul"],
      days: ["Yom Rishon", "Yom Sheni", "Yom Shlishi", "Yom Revi'i", "Yom Chamishi", "Yom Shishi", "Yom Shabbat"],
      first_day_of_week: 0  // Sunday
    },
    "chinese": {
      name: "Chinese Calendar",
      months: ["Zhēngyuè", "Èryuè", "Sānyuè", "Sìyuè", "Wǔyuè", "Liùyuè",
                "Qīyuè", "Bāyuè", "Jiǔyuè", "Shíyuè", "Shíyīyuè", "Shí'èryuè"],
      days: ["Xīngqīrì", "Xīngqīyī", "Xīngqī'èr", "Xīngqīsān", "Xīngqīsì", "Xīngqīwǔ", "Xīngqīliù"],
      first_day_of_week: 1  // Monday
    }
  }
  
  // Test month names
  let get_month_name = |month, calendar_system| {
    if month >= 1 && month <= 12 {
      calendar_systems[calendar_system].months[month - 1]
    } else {
      "Invalid month"
    }
  }
  
  assert_eq(get_month_name(1, "gregorian"), "January")
  assert_eq(get_month_name(12, "gregorian"), "December")
  assert_eq(get_month_name(1, "islamic"), "Muharram")
  assert_eq(get_month_name(12, "islamic"), "Dhu al-Hijjah")
  assert_eq(get_month_name(1, "hebrew"), "Tishrei")
  assert_eq(get_month_name(7, "hebrew"), "Nisan")
  assert_eq(get_month_name(1, "chinese"), "Zhēngyuè")
  assert_eq(get_month_name(12, "chinese"), "Shí'èryuè")
  
  // Test day names
  let get_day_name = |day, calendar_system| {
    if day >= 0 && day <= 6 {
      calendar_systems[calendar_system].days[day]
    } else {
      "Invalid day"
    }
  }
  
  assert_eq(get_day_name(0, "gregorian"), "Sunday")
  assert_eq(get_day_name(6, "gregorian"), "Saturday")
  assert_eq(get_day_name(0, "islamic"), "Al-Ahad")
  assert_eq(get_day_name(5, "islamic"), "Al-Jum'a")
  assert_eq(get_day_name(6, "hebrew"), "Yom Shabbat")
  assert_eq(get_day_name(0, "chinese"), "Xīngqīrì")
  
  // Test first day of week
  let get_first_day_of_week = |calendar_system| {
    calendar_systems[calendar_system].first_day_of_week
  }
  
  assert_eq(get_first_day_of_week("gregorian"), 0)  // Sunday
  assert_eq(get_first_day_of_week("islamic"), 6)    // Saturday
  assert_eq(get_first_day_of_week("hebrew"), 0)     // Sunday
  assert_eq(get_first_day_of_week("chinese"), 1)     // Monday
  
  // Test week start/end based on locale
  let get_week_start_end = |locale| {
    if locale == "en-US" || locale == "he-IL" {
      (0, 6)  // Sunday to Saturday
    } else if locale == "ar-SA" {
      (6, 5)  // Saturday to Friday
    } else {
      (1, 0)  // Monday to Sunday
    }
  }
  
  assert_eq(get_week_start_end("en-US"), (0, 6))  // Sunday to Saturday
  assert_eq(get_week_start_end("fr-FR"), (1, 0))  // Monday to Sunday
  assert_eq(get_week_start_end("ar-SA"), (6, 5))  // Saturday to Friday
  assert_eq(get_week_start_end("he-IL"), (0, 6))  // Sunday to Saturday
  
  // Test era names
  let get_era_name = |year, calendar_system| {
    if calendar_system == "gregorian" {
      if year > 0 {
        "AD"
      } else {
        "BC"
      }
    } else if calendar_system == "islamic" {
      "AH"
    } else if calendar_system == "hebrew" {
      "AM"
    } else if calendar_system == "chinese" {
      // Chinese calendar uses regnal years, simplified here
      if year >= 1984 {
        let cycle_start = 1984
        let position = year - cycle_start
        let cycle_number = position / 60 + 1
        "Cycle " + cycle_number.to_string()
      } else {
        "Ancient"
      }
    } else {
      ""
    }
  }
  
  assert_eq(get_era_name(2023, "gregorian"), "AD")
  assert_eq(get_era_name(-100, "gregorian"), "BC")
  assert_eq(get_era_name(1444, "islamic"), "AH")
  assert_eq(get_era_name(5783, "hebrew"), "AM")
  assert_eq(get_era_name(2023, "chinese"), "Cycle 1")
  
  // Test date formatting with different calendars
  let format_date_with_calendar = |year, month, day, calendar_system, locale| {
    let month_name = get_month_name(month, calendar_system)
    let day_name = get_day_name(day % 7, calendar_system)  // Simplified day calculation
    let era = get_era_name(year, calendar_system)
    
    if locale == "en-US" {
      month_name + " " + day.to_string() + ", " + year.to_string() + " " + era
    } else if locale == "fr-FR" {
      day.to_string() + " " + month_name + " " + year.to_string() + " " + era
    } else if locale == "ja-JP" {
      year.to_string() + "年" + month.to_string() + "月" + day.to_string() + "日"
    } else {
      year.to_string() + "-" + month.to_string() + "-" + day.to_string()
    }
  }
  
  assert_eq(format_date_with_calendar(2023, 12, 25, "gregorian", "en-US"), "December 25, 2023 AD")
  assert_eq(format_date_with_calendar(2023, 12, 25, "gregorian", "fr-FR"), "25 December 2023 AD")
  assert_eq(format_date_with_calendar(1444, 9, 10, "islamic", "en-US"), "Ramadan 10, 1444 AH")
  assert_eq(format_date_with_calendar(5783, 1, 1, "hebrew", "en-US"), "Tishrei 1, 5783 AM")
  assert_eq(format_date_with_calendar(2023, 12, 25, "gregorian", "ja-JP"), "2023年12月25日")
  
  // Test time zone handling
  let time_zones = {
    "UTC": { offset: 0, name: "Coordinated Universal Time" },
    "EST": { offset: -5, name: "Eastern Standard Time" },
    "PST": { offset: -8, name: "Pacific Standard Time" },
    "CET": { offset: 1, name: "Central European Time" },
    "JST": { offset: 9, name: "Japan Standard Time" },
    "GST": { offset: 4, name: "Gulf Standard Time" }
  }
  
  let convert_time_zone = |hour, minute, from_tz, to_tz| {
    let from_offset = time_zones[from_tz].offset
    let to_offset = time_zones[to_tz].offset
    
    // Convert to UTC first
    let utc_hour = hour - from_offset
    
    // Then convert to target timezone
    let target_hour = utc_hour + to_offset
    
    // Handle 24-hour wrap around
    let adjusted_hour = if target_hour >= 24 {
      target_hour - 24
    } else if target_hour < 0 {
      target_hour + 24
    } else {
      target_hour
    }
    
    (adjusted_hour, minute)
  }
  
  // Test time zone conversion
  assert_eq(convert_time_zone(12, 0, "EST", "UTC"), (17, 0))
  assert_eq(convert_time_zone(12, 0, "UTC", "EST"), (7, 0))
  assert_eq(convert_time_zone(12, 0, "JST", "UTC"), (3, 0))
  assert_eq(convert_time_zone(12, 0, "UTC", "JST"), (21, 0))
  assert_eq(convert_time_zone(23, 30, "PST", "CET"), (8, 30))  // Crosses midnight
}

// Test 6: Number and Currency Localization
test "number_and_currency_localization" {
  // Define number formatting rules
  let number_formats = {
    "en-US": {
      decimal_separator: ".",
      thousands_separator: ",",
      grouping_pattern: [3, 3],  // Group digits in 3s from the right
      negative_pattern: "-{number}",
      percent_pattern: "{number}%",
      currency_symbol: "$",
      currency_pattern: "{symbol}{number}",
      currency_decimal_places: 2
    },
    "fr-FR": {
      decimal_separator: ",",
      thousands_separator: " ",
      grouping_pattern: [3, 3],
      negative_pattern: "-{number}",
      percent_pattern: "{number} %",
      currency_symbol: "€",
      currency_pattern: "{number} {symbol}",
      currency_decimal_places: 2
    },
    "de-DE": {
      decimal_separator: ",",
      thousands_separator: ".",
      grouping_pattern: [3, 3],
      negative_pattern: "-{number}",
      percent_pattern: "{number}%",
      currency_symbol: "€",
      currency_pattern: "{number} {symbol}",
      currency_decimal_places: 2
    },
    "ja-JP": {
      decimal_separator: ".",
      thousands_separator: ",",
      grouping_pattern: [4, 4],  // Group in 10,000s
      negative_pattern: "-{number}",
      percent_pattern: "{number}%",
      currency_symbol: "¥",
      currency_pattern: "{symbol}{number}",
      currency_decimal_places: 0
    },
    "ar-SA": {
      decimal_separator: "٫",
      thousands_separator: "٬",
      grouping_pattern: [3, 3],
      negative_pattern: "{number}-",
      percent_pattern: "%{number}",
      currency_symbol: "ر.س",
      currency_pattern: "{symbol}{number}",
      currency_decimal_places: 2
    },
    "hi-IN": {
      decimal_separator: ".",
      thousands_separator: ",",
      grouping_pattern: [3, 2, 2],  // Indian numbering: 1,00,00,000
      negative_pattern: "-{number}",
      percent_pattern: "{number}%",
      currency_symbol: "₹",
      currency_pattern: "{symbol}{number}",
      currency_decimal_places: 2
    }
  }
  
  // Format number according to locale
  let format_number = |number, locale| {
    let format = number_formats[locale]
    let number_str = number.abs().to_string()
    let integer_part = number_str.split(".")[0]
    let decimal_part = if number_str.split(".").length() > 1 {
      number_str.split(".")[1]
    } else {
      ""
    }
    
    // Apply grouping
    let mut grouped_integer = ""
    let mut remaining = integer_part
    
    for group_size in format.grouping_pattern {
      if remaining.length() > group_size {
        let group = remaining.slice(remaining.length() - group_size, remaining.length())
        let prefix = remaining.slice(0, remaining.length() - group_size)
        grouped_integer = prefix + format.thousands_separator + group
        remaining = prefix
      } else {
        grouped_integer = remaining
        break
      }
    }
    
    let formatted_number = if decimal_part.length() > 0 {
      grouped_integer + format.decimal_separator + decimal_part
    } else {
      grouped_integer
    }
    
    // Apply negative pattern if needed
    if number < 0 {
      format.negative_pattern.replace("{number}", formatted_number)
    } else {
      formatted_number
    }
  }
  
  // Test number formatting
  assert_eq(format_number(1234.56, "en-US"), "1,234.56")
  assert_eq(format_number(1234.56, "fr-FR"), "1 234,56")
  assert_eq(format_number(1234.56, "de-DE"), "1.234,56")
  assert_eq(format_number(1234.56, "ja-JP"), "1,234.56")
  assert_eq(format_number(1234.56, "ar-SA"), "1٬234٫56")
  
  // Test large numbers
  assert_eq(format_number(1234567, "en-US"), "1,234,567")
  assert_eq(format_number(1234567, "hi-IN"), "12,34,567")  // Indian numbering
  
  // Test negative numbers
  assert_eq(format_number(-1234.56, "en-US"), "-1,234.56")
  assert_eq(format_number(-1234.56, "ar-SA"), "1٬234٫56-")
  
  // Test percentage formatting
  let format_percentage = |number, locale| {
    let format = number_formats[locale]
    let formatted_number = format_number(number * 100, locale)
    format.percent_pattern.replace("{number}", formatted_number)
  }
  
  assert_eq(format_percentage(0.1234, "en-US"), "12.34%")
  assert_eq(format_percentage(0.1234, "fr-FR"), "12,34 %")
  assert_eq(format_percentage(0.1234, "ar-SA"), "%12٫34")
  
  // Test currency formatting
  let format_currency = |amount, locale| {
    let format = number_formats[locale]
    
    // Round to appropriate decimal places
    let rounded_amount = if format.currency_decimal_places == 0 {
      amount.round()
    } else {
      let factor = 10.0.pow(format.currency_decimal_places.to_float())
      (amount * factor).round() / factor
    }
    
    let formatted_number = format_number(rounded_amount, locale)
    
    if format.currency_decimal_places == 0 {
      format.currency_pattern.replace("{symbol}", format.currency_symbol).replace("{number}", formatted_number.split(".")[0])
    } else {
      format.currency_pattern.replace("{symbol}", format.currency_symbol).replace("{number}", formatted_number)
    }
  }
  
  // Test currency formatting
  assert_eq(format_currency(1234.56, "en-US"), "$1,234.56")
  assert_eq(format_currency(1234.56, "fr-FR"), "1 234,56 €")
  assert_eq(format_currency(1234.56, "de-DE"), "1.234,56 €")
  assert_eq(format_currency(1234.56, "ja-JP"), "¥1,235")  // Rounded to 0 decimal places
  assert_eq(format_currency(1234.56, "ar-SA"), "ر.س1٬234٫56")
  assert_eq(format_currency(1234.56, "hi-IN"), "₹1,234.56")
  
  // Test unit formatting
  let format_unit = |value, unit, locale| {
    if locale == "en-US" {
      value.to_string() + " " + unit
    } else if locale == "fr-FR" {
      value.to_string() + " " + unit
    } else if locale == "ja-JP" {
      value.to_string() + unit
    } else {
      value.to_string() + " " + unit
    }
  }
  
  assert_eq(format_unit(25.5, "kg", "en-US"), "25.5 kg")
  assert_eq(format_unit(25.5, "kg", "fr-FR"), "25.5 kg")
  assert_eq(format_unit(25.5, "kg", "ja-JP"), "25.5kg")
  
  // Test measurement unit conversion
  let convert_measurement = |value, from_unit, to_unit| {
    let conversions = {
      "kg_to_lb": 2.20462,
      "lb_to_kg": 0.453592,
      "km_to_mile": 0.621371,
      "mile_to_km": 1.60934,
      "c_to_f": |c| c * 9.0 / 5.0 + 32.0,
      "f_to_c": |f| (f - 32.0) * 5.0 / 9.0
    }
    
    let conversion_key = from_unit + "_to_" + to_unit
    
    if conversions.contains(conversion_key) {
      let factor = conversions[conversion_key]
      if factor is Function {
        factor(value)
      } else {
        value * factor
      }
    } else {
      value  // No conversion available
    }
  }
  
  // Test measurement conversions
  assert_true(convert_measurement(10, "kg", "lb") > 22.0 && convert_measurement(10, "kg", "lb") < 22.1)
  assert_true(convert_measurement(100, "km", "mile") > 62.0 && convert_measurement(100, "km", "mile") < 62.2)
  assert_true(convert_measurement(0, "c", "f") > 31.9 && convert_measurement(0, "c", "f") < 32.1)
  assert_true(convert_measurement(32, "f", "c") > -0.1 && convert_measurement(32, "f", "c") < 0.1)
}

// Test 7: Collation and String Comparison
test "collation_and_string_comparison" {
  // Define collation rules
  let collation_rules = {
    "en": {
      name: "English",
      case_sensitive: false,
      diacritic_sensitive: false,
      order: ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
              "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]
    },
    "fr": {
      name: "French",
      case_sensitive: false,
      diacritic_sensitive: true,
      order: ["a", "à", "â", "ä", "b", "c", "ç", "d", "e", "é", "è", "ê", "ë", "f", "g",
              "h", "i", "î", "ï", "j", "k", "l", "m", "n", "o", "ô", "p", "q", "r", "s",
              "t", "u", "ù", "û", "ü", "v", "w", "x", "y", "ÿ", "z"]
    },
    "de": {
      name: "German",
      case_sensitive: false,
      diacritic_sensitive: true,
      order: ["a", "ä", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
              "n", "o", "ö", "p", "q", "r", "s", "ß", "t", "u", "ü", "v", "w", "x", "y", "z"]
    },
    "es": {
      name: "Spanish",
      case_sensitive: false,
      diacritic_sensitive: true,
      order: ["a", "á", "b", "c", "d", "e", "é", "f", "g", "h", "i", "í", "j", "k", "l", "m",
              "n", "ñ", "o", "ó", "p", "q", "r", "s", "t", "u", "ú", "ü", "v", "w", "x", "y", "ý", "ÿ", "z"]
    },
    "ja": {
      name: "Japanese",
      case_sensitive: false,
      diacritic_sensitive: false,
      order: ["あ", "い", "う", "え", "お", "か", "き", "く", "け", "こ", "が", "ぎ", "ぐ", "げ", "ご",
              "さ", "し", "す", "せ", "そ", "ざ", "じ", "ず", "ぜ", "ぞ", "た", "ち", "つ", "て", "と",
              "だ", "ぢ", "づ", "で", "ど", "な", "に", "ぬ", "ね", "の", "は", "ひ", "ふ", "へ", "ほ",
              "ば", "び", "ぶ", "べ", "ぼ", "ぱ", "ぴ", "ぷ", "ぺ", "ぽ", "ま", "み", "む", "め", "も",
              "や", "ゆ", "よ", "ら", "り", "る", "れ", "ろ", "わ", "を", "ん"]
    },
    "ar": {
      name: "Arabic",
      case_sensitive: false,
      diacritic_sensitive: false,
      order: ["ا", "ب", "ت", "ث", "ج", "ح", "خ", "د", "ذ", "ر", "ز", "س", "ش", "ص", "ض", "ط",
              "ظ", "ع", "غ", "ف", "ق", "ك", "ل", "م", "ن", "ه", "و", "ي"]
    }
  }
  
  // String comparison with locale
  let compare_strings = |str1, str2, locale| {
    let rules = collation_rules[locale]
    
    // Convert to lowercase if case-insensitive
    let s1 = if rules.case_sensitive { str1 } else { str1.to_lowercase() }
    let s2 = if rules.case_sensitive { str2 } else { str2.to_lowercase() }
    
    // Simple character-by-character comparison based on collation order
    let mut i = 0
    while i < s1.length() && i < s2.length() {
      let c1 = s1[i]
      let c2 = s2[i]
      
      if c1 != c2 {
        // Find positions in collation order
        let pos1 = rules.order.index_of(c1)
        let pos2 = rules.order.index_of(c2)
        
        if pos1 >= 0 && pos2 >= 0 {
          if pos1 < pos2 {
            return -1
          } else if pos1 > pos2 {
            return 1
          }
        }
        
        // Fallback to character code comparison
        if c1 < c2 {
          return -1
        } else {
          return 1
        }
      }
      
      i = i + 1
    }
    
    // If one string is a prefix of the other
    if s1.length() < s2.length() {
      -1
    } else if s1.length() > s2.length() {
      1
    } else {
      0
    }
  }
  
  // Test string comparison
  assert_eq(compare_strings("apple", "banana", "en"), -1)
  assert_eq(compare_strings("banana", "apple", "en"), 1)
  assert_eq(compare_strings("apple", "apple", "en"), 0)
  
  // Test case-insensitive comparison
  assert_eq(compare_strings("Apple", "apple", "en"), 0)
  assert_eq(compare_strings("APPLE", "apple", "en"), 0)
  
  // Test diacritic-sensitive comparison
  assert_eq(compare_strings("café", "cafe", "fr"), 1)  // café comes after cafe in French
  assert_eq(compare_strings("cafe", "café", "fr"), -1)
  
  assert_eq(compare_strings("café", "cafe", "en"), 0)  // Diacritics ignored in English
  
  // Test locale-specific ordering
  assert_eq(compare_strings("äpfel", "apfel", "de"), 1)  // ä comes after a in German
  
  // Test string sorting with locale
  let sort_strings = |strings, locale| {
    let mut sorted = strings
    sorted.sort_by(|a, b| compare_strings(a, b, locale))
    sorted
  }
  
  let english_words = ["apple", "Banana", "cherry", "date"]
  let sorted_en = sort_strings(english_words, "en")
  assert_eq(sorted_en, ["apple", "Banana", "cherry", "date"])
  
  let french_words = ["café", "cote", "côte", "coté"]
  let sorted_fr = sort_strings(french_words, "fr")
  // In French, diacritics matter: cote < coté < côte < café
  assert_eq(sorted_fr, ["cote", "coté", "côte", "café"])
  
  let german_words = ["apfel", "äpfel", "Äpfel", "banana"]
  let sorted_de = sort_strings(german_words, "de")
  // In German, ä comes after a: apfel < äpfel < Äpfel < banana
  assert_eq(sorted_de, ["apfel", "äpfel", "Äpfel", "banana"])
  
  // Test search with locale
  let search_strings = |strings, pattern, locale| {
    let mut matches = []
    let rules = collation_rules[locale]
    
    for str in strings {
      let s = if rules.case_sensitive { str } else { str.to_lowercase() }
      let p = if rules.case_sensitive { pattern } else { pattern.to_lowercase() }
      
      if s.contains(p) {
        matches.push(str)
      }
    }
    
    matches
  }
  
  let search_strings = ["café", "Café", "cafe", "CAFÉ", "bistro"]
  let matches_en = search_strings(search_strings, "cafe", "en")
  assert_eq(matches_en.length(), 4)  // Case-insensitive, diacritic-insensitive
  
  let matches_fr = search_strings(search_strings, "cafe", "fr")
  assert_eq(matches_fr.length(), 2)  // Case-insensitive, diacritic-sensitive
  
  // Test accent-insensitive search
  let accent_insensitive_search = |strings, pattern| {
    let mut matches = []
    
    for str in strings {
      // Remove diacritics for comparison
      let normalized_str = str.normalize("NFD").replace(/[\u0300-\u036f]/g, "")
      let normalized_pattern = pattern.normalize("NFD").replace(/[\u0300-\u036f]/g, "")
      
      if normalized_str.contains(normalized_pattern) {
        matches.push(str)
      }
    }
    
    matches
  }
  
  let accent_strings = ["café", "resume", "naïve", "cooperate", "fjord"]
  let matches = accent_insensitive_search(accent_strings, "cafe")
  assert_eq(matches, ["café"])
  
  let matches2 = accent_insensitive_search(accent_strings, "naive")
  assert_eq(matches2, ["naïve"])
}

// Test 8: Pluralization and Grammar Rules
test "pluralization_and_grammar_rules" {
  // Define pluralization rules
  let pluralization_rules = {
    "en": {
      name: "English",
      rules: [
        { condition: |n| n == 1, form: "one" },
        { condition: |n| n != 1, form: "other" }
      ]
    },
    "fr": {
      name: "French",
      rules: [
        { condition: |n| n >= 0 && n < 2, form: "one" },
        { condition: |n| n >= 2, form: "other" }
      ]
    },
    "ja": {
      name: "Japanese",
      rules: [
        { condition: |n| true, form: "other" }  // No plural forms
      ]
    },
    "ar": {
      name: "Arabic",
      rules: [
        { condition: |n| n == 0, form: "zero" },
        { condition: |n| n == 1, form: "one" },
        { condition: |n| n == 2, form: "two" },
        { condition: |n| n % 100 >= 3 && n % 100 <= 10, form: "few" },
        { condition: |n| n % 100 >= 11 && n % 100 <= 99, form: "many" },
        { condition: |n| true, form: "other" }
      ]
    },
    "ru": {
      name: "Russian",
      rules: [
        { condition: |n| n % 10 == 1 && n % 100 != 11, form: "one" },
        { condition: |n| n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 10 || n % 100 >= 20), form: "few" },
        { condition: |n| true, form: "many" }
      ]
    },
    "pl": {
      name: "Polish",
      rules: [
        { condition: |n| n == 1, form: "one" },
        { condition: |n| n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 10 || n % 100 >= 20), form: "few" },
        { condition: |n| true, form: "many" }
      ]
    }
  }
  
  // Get plural form for a number
  let get_plural_form = |number, locale| {
    let rules = pluralization_rules[locale]
    
    for rule in rules.rules {
      if rule.condition(number) {
        return rule.form
      }
    }
    
    "other"  // Default fallback
  }
  
  // Test plural forms
  assert_eq(get_plural_form(1, "en"), "one")
  assert_eq(get_plural_form(2, "en"), "other")
  assert_eq(get_plural_form(0, "en"), "other")
  
  assert_eq(get_plural_form(0, "fr"), "one")
  assert_eq(get_plural_form(1, "fr"), "one")
  assert_eq(get_plural_form(2, "fr"), "other")
  
  assert_eq(get_plural_form(5, "ja"), "other")  // Always "other" in Japanese
  
  assert_eq(get_plural_form(0, "ar"), "zero")
  assert_eq(get_plural_form(1, "ar"), "one")
  assert_eq(get_plural_form(2, "ar"), "two")
  assert_eq(get_plural_form(5, "ar"), "few")
  assert_eq(get_plural_form(15, "ar"), "many")
  
  assert_eq(get_plural_form(1, "ru"), "one")
  assert_eq(get_plural_form(2, "ru"), "few")
  assert_eq(get_plural_form(5, "ru"), "many")
  assert_eq(get_plural_form(11, "ru"), "many")
  
  // Define pluralized strings
  let pluralized_strings = {
    "en": {
      "items": {
        "one": "{count} item",
        "other": "{count} items"
      },
      "people": {
        "one": "{count} person",
        "other": "{count} people"
      },
      "files": {
        "one": "{count} file",
        "other": "{count} files"
      }
    },
    "fr": {
      "items": {
        "one": "{count} élément",
        "other": "{count} éléments"
      },
      "people": {
        "one": "{count} personne",
        "other": "{count} personnes"
      },
      "files": {
        "one": "{count} fichier",
        "other": "{count} fichiers"
      }
    },
    "ar": {
      "items": {
        "zero": "لا يوجد عناصر",
        "one": "عنصر واحد",
        "two": "عنصران",
        "few": "{count} عناصر",
        "many": "{count} عنصر",
        "other": "{count} عنصر"
      },
      "people": {
        "zero": "لا يوجد أشخاص",
        "one": "شخص واحد",
        "two": "شخصان",
        "few": "{count} أشخاص",
        "many": "{count} شخص",
        "other": "{count} شخص"
      }
    }
  }
  
  // Get pluralized string
  let get_pluralized_string = |key, count, locale| {
    let plural_form = get_plural_form(count, locale)
    
    if pluralized_strings.contains(locale) && 
       pluralized_strings[locale].contains(key) &&
       pluralized_strings[locale][key].contains(plural_form) {
      let template = pluralized_strings[locale][key][plural_form]
      template.replace("{count}", count.to_string())
    } else {
      key + " (" + count.to_string() + ")"
    }
  }
  
  // Test pluralized strings
  assert_eq(get_pluralized_string("items", 1, "en"), "1 item")
  assert_eq(get_pluralized_string("items", 5, "en"), "5 items")
  
  assert_eq(get_pluralized_string("items", 1, "fr"), "1 élément")
  assert_eq(get_pluralized_string("items", 5, "fr"), "5 éléments")
  
  assert_eq(get_pluralized_string("items", 0, "ar"), "لا يوجد عناصر")
  assert_eq(get_pluralized_string("items", 1, "ar"), "عنصر واحد")
  assert_eq(get_pluralized_string("items", 2, "ar"), "عنصران")
  assert_eq(get_pluralized_string("items", 5, "ar"), "5 عناصر")
  assert_eq(get_pluralized_string("items", 15, "ar"), "15 عنصر")
  
  // Test gender agreement
  let gender_rules = {
    "fr": {
      "article": {
        "masculine": "le",
        "feminine": "la"
      },
      "adjective": {
        "masculine": "beau",
        "feminine": "belle"
      }
    },
    "es": {
      "article": {
        "masculine": "el",
        "feminine": "la"
      },
      "adjective": {
        "masculine": "hermoso",
        "feminine": "hermosa"
      }
    },
    "de": {
      "article": {
        "masculine": "der",
        "feminine": "die"
      },
      "adjective": {
        "masculine": "schön",
        "feminine": "schöne"
      }
    }
  }
  
  // Get gendered string
  let get_gendered_string = |key, gender, locale| {
    if gender_rules.contains(locale) && 
       gender_rules[locale].contains(key) &&
       gender_rules[locale][key].contains(gender) {
      gender_rules[locale][key][gender]
    } else {
      key + "_" + gender
    }
  }
  
  // Test gender agreement
  assert_eq(get_gendered_string("article", "masculine", "fr"), "le")
  assert_eq(get_gendered_string("article", "feminine", "fr"), "la")
  assert_eq(get_gendered_string("adjective", "masculine", "fr"), "beau")
  assert_eq(get_gendered_string("adjective", "feminine", "fr"), "belle")
  
  assert_eq(get_gendered_string("article", "masculine", "es"), "el")
  assert_eq(get_gendered_string("article", "feminine", "es"), "la")
  assert_eq(get_gendered_string("adjective", "masculine", "es"), "hermoso")
  assert_eq(get_gendered_string("adjective", "feminine", "es"), "hermosa")
  
  // Test ordinal numbers
  let get_ordinal = |number, locale| {
    if locale == "en" {
      if number % 100 >= 11 && number % 100 <= 13 {
        number.to_string() + "th"
      } else if number % 10 == 1 {
        number.to_string() + "st"
      } else if number % 10 == 2 {
        number.to_string() + "nd"
      } else if number % 10 == 3 {
        number.to_string() + "rd"
      } else {
        number.to_string() + "th"
      }
    } else if locale == "fr" {
      if number == 1 {
        "1er"
      } else {
        number.to_string() + "ème"
      }
    } else if locale == "es" {
      if number == 1 || number == 3 {
        number.to_string() + "er"
      } else if number == 2 {
        number.to_string() + "º"
      } else {
        number.to_string() + "º"
      }
    } else {
      number.to_string()
    }
  }
  
  // Test ordinals
  assert_eq(get_ordinal(1, "en"), "1st")
  assert_eq(get_ordinal(2, "en"), "2nd")
  assert_eq(get_ordinal(3, "en"), "3rd")
  assert_eq(get_ordinal(4, "en"), "4th")
  assert_eq(get_ordinal(11, "en"), "11th")
  assert_eq(get_ordinal(21, "en"), "21st")
  
  assert_eq(get_ordinal(1, "fr"), "1er")
  assert_eq(get_ordinal(2, "fr"), "2ème")
  assert_eq(get_ordinal(10, "fr"), "10ème")
  
  assert_eq(get_ordinal(1, "es"), "1er")
  assert_eq(get_ordinal(2, "es"), "2º")
  assert_eq(get_ordinal(3, "es"), "3er")
}

// Test 9: Input Method and Keyboard Layout
test "input_method_and_keyboard_layout" {
  // Define keyboard layouts
  let keyboard_layouts = {
    "en-US": {
      name: "QWERTY (US)",
      layout: [
        ["`", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "="],
        ["q", "w", "e", "r", "t", "y", "u", "i", "o", "p", "[", "]", "\\"],
        ["a", "s", "d", "f", "g", "h", "j", "k", "l", ";", "'"],
        ["z", "x", "c", "v", "b", "n", "m", ",", ".", "/"]
      ],
      shift_layout: [
        ["~", "!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "_", "+"],
        ["Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P", "{", "}", "|"],
        ["A", "S", "D", "F", "G", "H", "J", "K", "L", ":", "\""],
        ["Z", "X", "C", "V", "B", "N", "M", "<", ">", "?"]
      ]
    },
    "fr-FR": {
      name: "AZERTY (French)",
      layout: [
        ["²", "&", "é", "\"", "'", "(", "-", "è", "_", "ç", "à", ")", "="],
        ["a", "z", "e", "r", "t", "y", "u", "i", "o", "p", "^", "$"],
        ["q", "s", "d", "f", "g", "h", "j", "k", "l", "m", "ù"],
        ["w", "x", "c", "v", "b", "n", ",", ";", ":", "!"]
      ],
      shift_layout: [
        ["~", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "+"],
        ["A", "Z", "E", "R", "T", "Y", "U", "I", "O", "P", "¨", "£"],
        ["Q", "S", "D", "F", "G", "H", "J", "K", "L", "M", "%"],
        ["W", "X", "C", "V", "B", "N", "?", ".", "/", "§"]
      ]
    },
    "de-DE": {
      name: "QWERTZ (German)",
      layout: [
        ["^", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "ß", "´"],
        ["q", "w", "e", "r", "t", "z", "u", "i", "o", "p", "ü", "+"],
        ["a", "s", "d", "f", "g", "h", "j", "k", "l", "ö", "ä", "#"],
        ["y", "x", "c", "v", "b", "n", "m", ",", ".", "-"]
      ],
      shift_layout: [
        ["°", "!", "\"", "§", "$", "%", "&", "/", "(", ")", "=", "?", "`"],
        ["Q", "W", "E", "R", "T", "Z", "U", "I", "O", "P", "Ü", "*"],
        ["A", "S", "D", "F", "G", "H", "J", "K", "L", "Ö", "Ä", "'"],
        ["Y", "X", "C", "V", "B", "N", "M", ";", ":", "_"]
      ]
    },
    "ja-JP": {
      name: "JIS (Japanese)",
      layout: [
        ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "^", "\\"],
        ["q", "w", "e", "r", "t", "y", "u", "i", "o", "p", "@", "["],
        ["a", "s", "d", "f", "g", "h", "j", "k", "l", ";", "]", "]"],
        ["z", "x", "c", "v", "b", "n", "m", ",", ".", "/"]
      ],
      shift_layout: [
        ["!", "\"", "#", "$", "%", "&", "'", "(", ")", "~", "=", "~", "|"],
        ["Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P", "`", "{"],
        ["A", "S", "D", "F", "G", "H", "J", "K", "L", ":", "{", "}"],
        ["Z", "X", "C", "V", "B", "N", "M", "<", ">", "?"]
      ]
    },
    "ar-SA": {
      name: "Arabic",
      layout: [
        ["ذ", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "="],
        ["ض", "ص", "ث", "ق", "ف", "غ", "ع", "ه", "خ", "ح", "ج", "د"],
        ["ش", "س", "ي", "ب", "ل", "ا", "ت", "ن", "م", "ك", "ط"],
        ["ئ", "ء", "ؤ", "ر", "لا", "ى", "ة", "و", "ز", "ظ"]
      ],
      shift_layout: [
        ["ّ", "!", "@", "#", "$", "%", "^", "&", "*", ")", "(", "_", "+"],
        ["َ", "ً", "ُ", "ٌ", "ِ", "ٍ", "ْ", "]", "[", "}", "{", ">", "<"],
        ["~", "،", "/", ":", "\"", "'", ">", "<", ",", ".", "؟"],
        ["|", "ـ", "،", "/", "؛", "ً", "ً", "ً", "ً", "ً"]
      ]
    }
  }
  
  // Simulate key press
  let simulate_key_press = |key, shift, layout| {
    let keyboard = keyboard_layouts[layout]
    
    // Find the key in the layout
    for row in keyboard.layout {
      for i = 0; i < row.length(); i = i + 1 {
        if row[i] == key {
          if shift {
            return keyboard.shift_layout[row.length() - 1][i]
          } else {
            return row[i]
          }
        }
      }
    }
    
    key  // Not found, return original
  }
  
  // Test key press simulation
  assert_eq(simulate_key_press("a", false, "en-US"), "a")
  assert_eq(simulate_key_press("a", true, "en-US"), "A")
  assert_eq(simulate_key_press("1", false, "en-US"), "1")
  assert_eq(simulate_key_press("1", true, "en-US"), "!")
  
  // Test different layouts
  assert_eq(simulate_key_press("a", false, "fr-FR"), "q")  // A key produces Q in French layout
  assert_eq(simulate_key_press("q", false, "fr-FR"), "a")  // Q key produces A in French layout
  assert_eq(simulate_key_press("z", false, "de-DE"), "y")  // Z key produces Y in German layout
  assert_eq(simulate_key_press("y", false, "de-DE"), "z")  // Y key produces Z in German layout
  
  // Test input method simulation
  let input_method = |layout, input_sequence| {
    let mut result = ""
    
    for char in input_sequence {
      // Simple simulation: just append the character
      result = result + char
    }
    
    result
  }
  
  // Test input method
  assert_eq(input_method("en-US", "hello"), "hello")
  assert_eq(input_method("fr-FR", "hello"), "hello")
  
  // Test dead key combinations
  let simulate_dead_key = |dead_key, base_key, layout| {
    // Simplified dead key simulation
    if dead_key == "`" && base_key == "a" {
      "à"
    } else if dead_key == "^" && base_key == "a" {
      "â"
    } else if dead_key == "¨" && base_key == "a" {
      "ä"
    } else if dead_key == "~" && base_key == "a" {
      "ã"
    } else if dead_key == "´" && base_key == "a" {
      "á"
    } else {
      base_key  // No combination
    }
  }
  
  // Test dead key combinations
  assert_eq(simulate_dead_key("`", "a", "en-US"), "à")
  assert_eq(simulate_dead_key("^", "a", "en-US"), "â")
  assert_eq(simulate_dead_key("¨", "a", "en-US"), "ä")
  assert_eq(simulate_dead_key("~", "a", "en-US"), "ã")
  assert_eq(simulate_dead_key("´", "a", "en-US"), "á")
  
  // Test IME (Input Method Editor) simulation
  let ime_simulation = |input_sequence, target_language| {
    if target_language == "ja" {
      // Simplified Japanese IME simulation
      if input_sequence == "konnichiha" {
        "こんにちは"
      } else if input_sequence == "arigatou" {
        "ありがとう"
      } else if input_sequence == "sayonara" {
        "さようなら"
      } else {
        input_sequence
      }
    } else if target_language == "zh" {
      // Simplified Chinese IME simulation
      if input_sequence == "nihao" {
        "你好"
      } else if input_sequence == "xiexie" {
        "谢谢"
      } else if input_sequence == "zaijian" {
        "再见"
      } else {
        input_sequence
      }
    } else if target_language == "ko" {
      // Simplified Korean IME simulation
      if input_sequence == "annyeong" {
        "안녕"
      } else if input_sequence == "gamsahamnida" {
        "감사합니다"
      } else if input_sequence == "annyeonghi gyeseyo" {
        "안녕히 계세요"
      } else {
        input_sequence
      }
    } else {
      input_sequence
    }
  }
  
  // Test IME
  assert_eq(ime_simulation("konnichiha", "ja"), "こんにちは")
  assert_eq(ime_simulation("nihao", "zh"), "你好")
  assert_eq(ime_simulation("annyeong", "ko"), "안녕")
  
  // Test auto-completion
  let auto_complete = |partial_input, dictionary| {
    let mut suggestions = []
    
    for word in dictionary {
      if word.starts_with(partial_input) {
        suggestions.push(word)
      }
    }
    
    suggestions
  }
  
  // Test auto-completion
  let english_words = ["hello", "help", "helmet", "helicopter", "hell"]
  let suggestions = auto_complete("hel", english_words)
  assert_eq(suggestions, ["hello", "help", "helmet", "helicopter", "hell"])
  
  let japanese_words = ["こんにちは", "こんばんは", "ありがとう", "ありがとうございます"]
  let suggestions_ja = auto_complete("こん", japanese_words)
  assert_eq(suggestions_ja, ["こんにちは", "こんばんは"])
}

// Test 10: Cultural Adaptation and Localization
test "cultural_adaptation_and_localization" {
  // Define cultural preferences
  let cultural_preferences = {
    "US": {
      date_format: "MM/DD/YYYY",
      time_format: "12h",
      first_day_of_week: "Sunday",
      measurement_system: "imperial",
      paper_size: "Letter",
      currency: "USD",
      driving_side: "right",
      weekend_days: ["Saturday", "Sunday"],
      common_greeting: "How are you?",
      personal_space: "arm's length",
      eye_contact: "direct",
      gestures: {
        "thumbs_up": "positive",
        "ok": "positive",
        "peace": "positive"
      }
    },
    "UK": {
      date_format: "DD/MM/YYYY",
      time_format: "12h",
      first_day_of_week: "Monday",
      measurement_system: "imperial",
      paper_size: "A4",
      currency: "GBP",
      driving_side: "left",
      weekend_days: ["Saturday", "Sunday"],
      common_greeting: "How do you do?",
      personal_space: "arm's length",
      eye_contact: "direct",
      gestures: {
        "thumbs_up": "positive",
        "ok": "positive",
        "peace": "positive (palm out), negative (palm in)"
      }
    },
    "France": {
      date_format: "DD/MM/YYYY",
      time_format: "24h",
      first_day_of_week: "Monday",
      measurement_system: "metric",
      paper_size: "A4",
      currency: "EUR",
      driving_side: "right",
      weekend_days: ["Saturday", "Sunday"],
      common_greeting: "Bonjour!",
      personal_space: "closer than US",
      eye_contact: "direct but brief",
      gestures: {
        "thumbs_up": "positive",
        "ok": "zero/worthless",
        "peace": "positive"
      }
    },
    "Japan": {
      date_format: "YYYY/MM/DD",
      time_format: "24h",
      first_day_of_week: "Sunday",
      measurement_system: "metric",
      paper_size: "A4",
      currency: "JPY",
      driving_side: "left",
      weekend_days: ["Saturday", "Sunday"],
      common_greeting: "こんにちは (Konnichiwa)",
      personal_space: "more distance",
      eye_contact: "less direct",
      gestures: {
        "thumbs_up": "positive",
        "ok": "positive/money",
        "peace": "positive (in photos)"
      }
    },
    "Saudi Arabia": {
      date_format: "DD/MM/YYYY",
      time_format: "12h",
      first_day_of_week: "Saturday",
      measurement_system: "metric",
      paper_size: "A4",
      currency: "SAR",
      driving_side: "right",
      weekend_days: ["Friday", "Saturday"],
      common_greeting: "السلام عليكم (As-salamu alaykum)",
      personal_space: "same gender closer, opposite gender farther",
      eye_contact: "less direct, especially between genders",
      gestures: {
        "thumbs_up": "positive",
        "ok": "positive",
        "peace": "positive"
      }
    },
    "India": {
      date_format: "DD/MM/YYYY",
      time_format: "24h",
      first_day_of_week: "Monday",
      measurement_system: "metric",
      paper_size: "A4",
      currency: "INR",
      driving_side: "left",
      weekend_days: ["Sunday"],
      common_greeting: "नमस्ते (Namaste)",
      personal_space: "closer than Western",
      eye_contact: "less direct",
      gestures: {
        "thumbs_up": "positive",
        "ok": "positive",
        "peace": "positive"
      }
    }
  }
  
  // Test cultural adaptation
  let adapt_to_culture = |country| {
    cultural_preferences[country]
  }
  
  // Test cultural preferences
  let us_prefs = adapt_to_culture("US")
  assert_eq(us_prefs.date_format, "MM/DD/YYYY")
  assert_eq(us_prefs.time_format, "12h")
  assert_eq(us_prefs.measurement_system, "imperial")
  assert_eq(us_prefs.driving_side, "right")
  assert_eq(us_prefs.weekend_days, ["Saturday", "Sunday"])
  
  let uk_prefs = adapt_to_culture("UK")
  assert_eq(uk_prefs.date_format, "DD/MM/YYYY")
  assert_eq(uk_prefs.measurement_system, "imperial")
  assert_eq(uk_prefs.driving_side, "left")
  assert_eq(uk_prefs.gestures["peace"], "positive (palm out), negative (palm in)")
  
  let france_prefs = adapt_to_culture("France")
  assert_eq(france_prefs.time_format, "24h")
  assert_eq(france_prefs.measurement_system, "metric")
  assert_eq(france_prefs.gestures["ok"], "zero/worthless")
  
  let japan_prefs = adapt_to_culture("Japan")
  assert_eq(japan_prefs.date_format, "YYYY/MM/DD")
  assert_eq(japan_prefs.driving_side, "left")
  assert_eq(japan_prefs.personal_space, "more distance")
  
  let saudi_prefs = adapt_to_culture("Saudi Arabia")
  assert_eq(saudi_prefs.first_day_of_week, "Saturday")
  assert_eq(saudi_prefs.weekend_days, ["Friday", "Saturday"])
  assert_eq(saudi_prefs.gestures["thumbs_up"], "positive")
  
  let india_prefs = adapt_to_culture("India")
  assert_eq(india_prefs.weekend_days, ["Sunday"])
  assert_eq(india_prefs.driving_side, "left")
  assert_eq(india_prefs.common_greeting, "नमस्ते (Namaste)")
  
  // Test color symbolism
  let color_symbolism = {
    "Western": {
      "red": ["love", "danger", "stop", "passion"],
      "blue": ["trust", "calm", "sadness", "authority"],
      "green": ["nature", "growth", "money", "envy"],
      "white": ["purity", "peace", "cleanliness", "sterility"],
      "black": ["death", "elegance", "evil", "formality"],
      "yellow": ["happiness", "caution", "cowardice"]
    },
    "Eastern": {
      "red": ["luck", "prosperity", "happiness", "celebration"],
      "blue": ["immortality", "healing", "calm"],
      "green": ["nature", "health", "harmony", "youth"],
      "white": ["death", "mourning", "purity"],
      "black": ["water", "north", "evil", "knowledge"],
      "yellow": ["royalty", "power", "sacred", "earth"]
    },
    "Middle Eastern": {
      "red": ["danger", "evil", "energy"],
      "blue": ["safety", "protection", "spirituality"],
      "green": ["fertility", "wealth", "strength", "Islam"],
      "white": ["purity", "peace", "mourning"],
      "black": ["rebirth", "evil", "mystery"],
      "yellow": ["happiness", "prosperity", "sickness"]
    },
    "Indian": {
      "red": ["purity", "sensuality", "marriage", "festivals"],
      "blue": ["Krishna", "calm", "infinity"],
      "green": ["fertility", "prosperity", "nature", "Islam"],
      "white": ["purity", "peace", "knowledge"],
      "black": ["evil", "negativity", "anger"],
      "yellow": ["knowledge", "learning", "happiness", "merchants"]
    }
  }
  
  // Test color symbolism
  let get_color_meanings = |color, culture| {
    color_symbolism[culture][color]
  }
  
  assert_eq(get_color_meanings("red", "Western"), ["love", "danger", "stop", "passion"])
  assert_eq(get_color_meanings("red", "Eastern"), ["luck", "prosperity", "happiness", "celebration"])
  assert_eq(get_color_meanings("white", "Western"), ["purity", "peace", "cleanliness", "sterility"])
  assert_eq(get_color_meanings("white", "Eastern"), ["death", "mourning", "purity"])
  
  // Test number symbolism
  let number_symbolism = {
    "Western": {
      4: ["death", "unlucky"],
      7: ["luck", "perfection"],
      13: ["unlucky", "betrayal"],
      666: ["evil", "devil"]
    },
    "Chinese": {
      4: ["death", "unlucky"],
      8: ["prosperity", "wealth"],
      9: ["longevity", "eternity"],
      6: ["success", "smooth"]
    },
    "Japanese": {
      4: ["death", "unlucky"],
      9: ["suffering", "torture"],
      7: ["luck", "happiness"],
      8: ["prosperity", "wealth"]
    }
  }
  
  // Test number symbolism
  let get_number_meanings = |number, culture| {
    if number_symbolism.contains(culture) && number_symbolism[culture].contains(number) {
      number_symbolism[culture][number]
    } else {
      []
    }
  }
  
  assert_eq(get_number_meanings(4, "Western"), ["death", "unlucky"])
  assert_eq(get_number_meanings(4, "Chinese"), ["death", "unlucky"])
  assert_eq(get_number_meanings(8, "Chinese"), ["prosperity", "wealth"])
  assert_eq(get_number_meanings(9, "Japanese"), ["suffering", "torture"])
  
  // Test name ordering
  let get_name_order = |culture| {
    if culture == "Western" || culture == "Middle Eastern" {
      "given_family"  // Given name then family name
    } else if culture == "Eastern" || culture == "Hungarian" {
      "family_given"  // Family name then given name
    } else {
      "given_family"  // Default
    }
  }
  
  assert_eq(get_name_order("Western"), "given_family")
  assert_eq(get_name_order("Eastern"), "family_given")
  assert_eq(get_name_order("Hungarian"), "family_given")
  
  // Test address formatting
  let format_address = |address, culture| {
    if culture == "Western" {
      address.street + "\n" + address.city + ", " + address.state + " " + address.postal_code + "\n" + address.country
    } else if culture == "Japanese" {
      address.postal_code + "\n" + address.state + address.city + "\n" + address.street + "\n" + address.country
    } else {
      address.street + "\n" + address.city + "\n" + address.country
    }
  }
  
  let us_address = {
    street: "123 Main St",
    city: "New York",
    state: "NY",
    postal_code: "10001",
    country: "USA"
  }
  
  let japan_address = {
    street: "〒100-0001 東京都千代田区千代田1-1",
    city: "Chiyoda",
    state: "Tokyo",
    postal_code: "100-0001",
    country: "Japan"
  }
  
  let us_formatted = format_address(us_address, "Western")
  assert_eq(us_formatted, "123 Main St\nNew York, NY 10001\nUSA")
  
  let japan_formatted = format_address(japan_address, "Japanese")
  assert_eq(japan_formatted, "100-0001\nTokyo Chiyoda\n〒100-0001 東京都千代田区千代田1-1\nJapan")
  
  // Test phone number formatting
  let format_phone_number = |phone_number, country| {
    if country == "US" {
      // Format: (XXX) XXX-XXXX
      if phone_number.length() == 10 {
        "(" + phone_number.slice(0, 3) + ") " + phone_number.slice(3, 6) + "-" + phone_number.slice(6, 10)
      } else {
        phone_number
      }
    } else if country == "UK" {
      // Format: XXXX XXXXXX
      if phone_number.length() == 10 {
        phone_number.slice(0, 4) + " " + phone_number.slice(4, 10)
      } else {
        phone_number
      }
    } else if country == "Japan" {
      // Format: XX-XXXX-XXXX
      if phone_number.length() == 10 {
        phone_number.slice(0, 2) + "-" + phone_number.slice(2, 6) + "-" + phone_number.slice(6, 10)
      } else {
        phone_number
      }
    } else {
      phone_number
    }
  }
  
  assert_eq(format_phone_number("1234567890", "US"), "(123) 456-7890")
  assert_eq(format_phone_number("1234567890", "UK"), "1234 567890")
  assert_eq(format_phone_number("1234567890", "Japan"), "12-3456-7890")
}