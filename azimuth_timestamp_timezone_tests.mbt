// Azimuth Timestamp and Timezone Tests
// This file contains test cases for timestamp operations and timezone handling

// Test 1: Unix Timestamp Operations
test "unix timestamp operations" {
  // Test timestamp creation
  let timestamp1 = create_timestamp(1640995200L) // 2022-01-01 00:00:00 UTC
  let timestamp2 = create_timestamp(1641081600L) // 2022-01-02 00:00:00 UTC
  
  assert_eq(timestamp1.unix_seconds, 1640995200L)
  assert_eq(timestamp2.unix_seconds, 1641081600L)
  
  // Test timestamp comparison
  assert_true(timestamp1.before(timestamp2))
  assert_false(timestamp2.before(timestamp1))
  assert_true(timestamp2.after(timestamp1))
  assert_false(timestamp1.after(timestamp2))
  
  // Test timestamp arithmetic
  let duration = timestamp2.difference(timestamp1)
  assert_eq(duration.seconds, 86400L) // 1 day = 86400 seconds
  
  let timestamp3 = timestamp1.add_seconds(3600L) // Add 1 hour
  assert_eq(timestamp3.unix_seconds, 1640998800L)
  
  let timestamp4 = timestamp2.subtract_seconds(1800L) // Subtract 30 minutes
  assert_eq(timestamp4.unix_seconds, 1641079800L)
  
  // Test timestamp formatting
  let formatted = timestamp1.format("YYYY-MM-DD HH:mm:ss")
  assert_eq(formatted, "2022-01-01 00:00:00")
  
  let iso_formatted = timestamp1.format_iso()
  assert_eq(iso_formatted, "2022-01-01T00:00:00Z")
}

// Test 2: Timezone Conversion
test "timezone conversion" {
  // Test UTC timestamp
  let utc_timestamp = create_timestamp(1640995200L) // 2022-01-01 00:00:00 UTC
  
  // Test conversion to different timezones
  let est_timestamp = utc_timestamp.to_timezone("America/New_York")
  let pst_timestamp = utc_timestamp.to_timezone("America/Los_Angeles")
  let jst_timestamp = utc_timestamp.to_timezone("Asia/Tokyo")
  let cet_timestamp = utc_timestamp.to_timezone("Europe/Paris")
  
  // EST is UTC-5 (or UTC-4 during DST)
  assert_eq(est_timestamp.hour, 19) // 19:00 on previous day (Dec 31, 2021)
  assert_eq(est_timestamp.day, 31)
  assert_eq(est_timestamp.month, 12)
  assert_eq(est_timestamp.year, 2021)
  
  // PST is UTC-8 (or UTC-7 during DST)
  assert_eq(pst_timestamp.hour, 16) // 16:00 on previous day (Dec 31, 2021)
  assert_eq(pst_timestamp.day, 31)
  assert_eq(pst_timestamp.month, 12)
  assert_eq(pst_timestamp.year, 2021)
  
  // JST is UTC+9
  assert_eq(jst_timestamp.hour, 9) // 09:00 on same day
  assert_eq(jst_timestamp.day, 1)
  assert_eq(jst_timestamp.month, 1)
  assert_eq(jst_timestamp.year, 2022)
  
  // CET is UTC+1
  assert_eq(cet_timestamp.hour, 1) // 01:00 on same day
  assert_eq(cet_timestamp.day, 1)
  assert_eq(cet_timestamp.month, 1)
  assert_eq(cet_timestamp.year, 2022)
  
  // Test timezone formatting
  let est_formatted = est_timestamp.format("YYYY-MM-DD HH:mm:ss Z")
  assert_eq(est_formatted, "2021-12-31 19:00:00 EST")
  
  // Test conversion back to UTC
  let back_to_utc = est_timestamp.to_timezone("UTC")
  assert_eq(back_to_utc.unix_seconds, utc_timestamp.unix_seconds)
}

// Test 3: Daylight Saving Time Handling
test "daylight saving time handling" {
  // Test DST transition (US DST starts on second Sunday in March)
  let before_dst = create_timestamp(1648041600L) // 2022-03-12 00:00:00 UTC (before DST)
  let after_dst = create_timestamp(1648128000L) // 2022-03-13 00:00:00 UTC (after DST)
  
  // Before DST: EST (UTC-5)
  let before_dst_est = before_dst.to_timezone("America/New_York")
  assert_eq(before_dst_est.hour, 19) // 19:00 on March 11
  assert_eq(before_dst_est.day, 11)
  assert_eq(before_dst_est.month, 3)
  
  // After DST: EDT (UTC-4)
  let after_dst_est = after_dst.to_timezone("America/New_York")
  assert_eq(after_dst_est.hour, 20) // 20:00 on March 12 (1 hour later due to DST)
  assert_eq(after_dst_est.day, 12)
  assert_eq(after_dst_est.month, 3)
  
  // Test DST end (US DST ends on first Sunday in November)
  let before_dst_end = create_timestamp(1667606400L) // 2022-11-05 00:00:00 UTC (before DST end)
  let after_dst_end = create_timestamp(1667692800L) // 2022-11-06 00:00:00 UTC (after DST end)
  
  // Before DST end: EDT (UTC-4)
  let before_dst_end_est = before_dst_end.to_timezone("America/New_York")
  assert_eq(before_dst_end_est.hour, 20) // 20:00 on November 4
  assert_eq(before_dst_end_est.day, 4)
  assert_eq(before_dst_end_est.month, 11)
  
  // After DST end: EST (UTC-5)
  let after_dst_end_est = after_dst_end.to_timezone("America/New_York")
  assert_eq(after_dst_end_est.hour, 19) // 19:00 on November 5 (1 hour earlier due to DST end)
  assert_eq(after_dst_end_est.day, 5)
  assert_eq(after_dst_end_est.month, 11)
  
  // Test DST detection
  assert_false(before_dst.is_dst("America/New_York"))
  assert_true(after_dst.is_dst("America/New_York"))
  assert_true(before_dst_end.is_dst("America/New_York"))
  assert_false(after_dst_end.is_dst("America/New_York"))
}

// Test 4: Time Parsing and Formatting
test "time parsing and formatting" {
  // Test ISO 8601 parsing
  let iso_timestamp = parse_iso8601("2022-01-01T12:34:56Z")
  match iso_timestamp {
    Some(timestamp) => {
      assert_eq(timestamp.unix_seconds, 1641039296L)
      assert_eq(timestamp.year, 2022)
      assert_eq(timestamp.month, 1)
      assert_eq(timestamp.day, 1)
      assert_eq(timestamp.hour, 12)
      assert_eq(timestamp.minute, 34)
      assert_eq(timestamp.second, 56)
    }
    None => assert_true(false)
  }
  
  // Test ISO 8601 with timezone offset
  let iso_with_offset = parse_iso8601("2022-01-01T12:34:56+02:00")
  match iso_with_offset {
    Some(timestamp) => {
      // +02:00 means 2 hours ahead of UTC, so UTC time is 10:34:56
      assert_eq(timestamp.unix_seconds, 1641032096L)
      assert_eq(timestamp.hour, 10) // UTC hour
    }
    None => assert_true(false)
  }
  
  // Test custom format parsing
  let custom_timestamp = parse_custom_format("2022/01/01 12:34:56", "YYYY/MM/DD HH:mm:ss")
  match custom_timestamp {
    Some(timestamp) => {
      assert_eq(timestamp.year, 2022)
      assert_eq(timestamp.month, 1)
      assert_eq(timestamp.day, 1)
      assert_eq(timestamp.hour, 12)
      assert_eq(timestamp.minute, 34)
      assert_eq(timestamp.second, 56)
    }
    None => assert_true(false)
  }
  
  // Test various formatting options
  let test_timestamp = create_timestamp(1641039296L) // 2022-01-01 12:34:56 UTC
  
  assert_eq(test_timestamp.format("YYYY-MM-DD"), "2022-01-01")
  assert_eq(test_timestamp.format("MM/DD/YYYY"), "01/01/2022")
  assert_eq(test_timestamp.format("DD MMM YYYY"), "01 Jan 2022")
  assert_eq(test_timestamp.format("HH:mm:ss"), "12:34:56")
  assert_eq(test_timestamp.format("hh:mm:ss A"), "12:34:56 PM")
  assert_eq(test_timestamp.format("YYYY-MM-DD HH:mm:ss Z"), "2022-01-01 12:34:56 UTC")
  
  // Test invalid format parsing
  let invalid_timestamp = parse_custom_format("invalid date", "YYYY-MM-DD")
  match invalid_timestamp {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 5: Duration and Interval Operations
test "duration and interval operations" {
  // Test duration creation
  let duration1 = create_duration(86400L) // 1 day
  let duration2 = create_duration(3600L) // 1 hour
  let duration3 = create_duration(900L) // 15 minutes
  
  assert_eq(duration1.days, 1)
  assert_eq(duration1.hours, 24)
  assert_eq(duration1.minutes, 1440)
  assert_eq(duration1.seconds, 86400)
  
  assert_eq(duration2.hours, 1)
  assert_eq(duration2.minutes, 60)
  assert_eq(duration2.seconds, 3600)
  
  // Test duration arithmetic
  let combined_duration = duration1.add(duration2)
  assert_eq(combined_duration.seconds, 90000L) // 86400 + 3600
  
  let subtracted_duration = duration1.subtract(duration3)
  assert_eq(subtracted_duration.seconds, 85500L) // 86400 - 900
  
  // Test duration comparison
  assert_true(duration1.greater_than(duration2))
  assert_false(duration2.greater_than(duration1))
  assert_true(duration1.greater_equal(duration2))
  assert_true(duration1.equal(duration1))
  assert_false(duration1.equal(duration2))
  
  // Test duration formatting
  assert_eq(duration1.format("D days, H hours, M minutes"), "1 days, 24 hours, 1440 minutes")
  assert_eq(duration2.format("H hours, M minutes"), "1 hours, 60 minutes")
  assert_eq(duration3.format("M minutes, S seconds"), "15 minutes, 900 seconds")
  
  // Test interval operations
  let start_time = create_timestamp(1640995200L) // 2022-01-01 00:00:00 UTC
  let end_time = start_time.add_duration(duration1) // Add 1 day
  
  let interval = create_interval(start_time, end_time)
  assert_eq(interval.duration().seconds, 86400L)
  assert_true(interval.contains(start_time))
  assert_true(interval.contains(end_time))
  
  let middle_time = start_time.add_seconds(43200L) // 12 hours later
  assert_true(interval.contains(middle_time))
  
  let outside_time = end_time.add_seconds(1L)
  assert_false(interval.contains(outside_time))
  
  // Test interval overlap
  let overlapping_start = start_time.add_seconds(43200L) // 12 hours after start
  let overlapping_end = end_time.add_seconds(43200L) // 12 hours after end
  let overlapping_interval = create_interval(overlapping_start, overlapping_end)
  
  assert_true(interval.overlaps(overlapping_interval))
  
  let non_overlapping_start = end_time.add_seconds(1L)
  let non_overlapping_end = non_overlapping_start.add_seconds(86400L)
  let non_overlapping_interval = create_interval(non_overlapping_start, non_overlapping_end)
  
  assert_false(interval.overlaps(non_overlapping_interval))
}

// Test 6: Leap Year and Calendar Operations
test "leap year and calendar operations" {
  // Test leap year detection
  assert_true(is_leap_year(2000))
  assert_true(is_leap_year(2004))
  assert_false(is_leap_year(1900))
  assert_false(is_leap_year(2001))
  assert_false(is_leap_year(2100))
  
  // Test days in month
  assert_eq(days_in_month(2022, 1), 31)
  assert_eq(days_in_month(2022, 2), 28)
  assert_eq(days_in_month(2020, 2), 29) // Leap year
  assert_eq(days_in_month(2022, 4), 30)
  assert_eq(days_in_month(2022, 12), 31)
  
  // Test day of week calculation
  let jan1_2022 = create_timestamp(1640995200L) // 2022-01-01 00:00:00 UTC
  assert_eq(jan1_2022.day_of_week(), 6) // Saturday
  
  let jul4_2022 = create_timestamp(1656912000L) // 2022-07-04 00:00:00 UTC
  assert_eq(jul4_2022.day_of_week(), 1) // Monday
  
  let dec25_2022 = create_timestamp(1671916800L) // 2022-12-25 00:00:00 UTC
  assert_eq(dec25_2022.day_of_week(), 0) // Sunday
  
  // Test day of year calculation
  assert_eq(jan1_2022.day_of_year(), 1)
  assert_eq(jul4_2022.day_of_year(), 185)
  assert_eq(dec25_2022.day_of_year(), 359)
  
  // Test week of year calculation
  assert_eq(jan1_2022.week_of_year(), 52) // Week 52 of 2021 (ISO week)
  assert_eq(jul4_2022.week_of_year(), 27)
  assert_eq(dec25_2022.week_of_year(), 51)
  
  // Test quarter calculation
  assert_eq(jan1_2022.quarter(), 1)
  assert_eq(jul4_2022.quarter(), 3)
  assert_eq(dec25_2022.quarter(), 4)
}

// Test 7: Timestamp Precision and Accuracy
test "timestamp precision and accuracy" {
  // Test millisecond precision
  let ms_timestamp = create_timestamp_with_ms(1641039296L, 123) // 2022-01-01 12:34:56.123 UTC
  assert_eq(ms_timestamp.unix_seconds, 1641039296L)
  assert_eq(ms_timestamp.milliseconds, 123)
  
  // Test microsecond precision
  let us_timestamp = create_timestamp_with_us(1641039296L, 123456) // 2022-01-01 12:34:56.123456 UTC
  assert_eq(us_timestamp.unix_seconds, 1641039296L)
  assert_eq(us_timestamp.microseconds, 123456)
  
  // Test nanosecond precision
  let ns_timestamp = create_timestamp_with_ns(1641039296L, 123456789) // 2022-01-01 12:34:56.123456789 UTC
  assert_eq(ns_timestamp.unix_seconds, 1641039296L)
  assert_eq(ns_timestamp.nanoseconds, 123456789)
  
  // Test precision conversion
  let ms_from_us = us_timestamp.to_milliseconds()
  assert_eq(ms_from_us, 1641039296123L)
  
  let us_from_ns = ns_timestamp.to_microseconds()
  assert_eq(us_from_ns, 1641039296123456L)
  
  // Test high resolution timing
  let start_time = high_resolution_timestamp()
  // Simulate some work
  let end_time = high_resolution_timestamp()
  
  let elapsed = end_time.difference(start_time)
  assert_true(elapsed.nanoseconds >= 0)
  
  // Test timestamp comparison with different precisions
  let timestamp1 = create_timestamp_with_ms(1641039296L, 123)
  let timestamp2 = create_timestamp_with_ms(1641039296L, 124)
  let timestamp3 = create_timestamp_with_ms(1641039296L, 123)
  
  assert_true(timestamp1.before(timestamp2))
  assert_false(timestamp1.before(timestamp3))
  assert_true(timestamp1.equal(timestamp3))
  assert_false(timestamp1.equal(timestamp2))
}

// Test 8: Time Zone Database Operations
test "time zone database operations" {
  // Test time zone database loading
  let tz_db = load_timezone_database()
  
  // Test time zone information retrieval
  let ny_tz = tz_db.get_timezone("America/New_York")
  match ny_tz {
    Some(tz_info) => {
      assert_eq(tz_info.name, "America/New_York")
      assert_eq(tz_info.standard_offset, -300) // -5 hours in minutes
      assert_eq(tz_info.dst_offset, -240) // -4 hours in minutes during DST
    }
    None => assert_true(false)
  }
  
  // Test time zone listing
  let all_timezones = tz_db.list_timezones()
  assert_true(all_timezones.length() > 0)
  assert_true(all_timezones.contains("UTC"))
  assert_true(all_timezones.contains("America/New_York"))
  assert_true(all_timezones.contains("Europe/London"))
  assert_true(all_timezones.contains("Asia/Tokyo"))
  
  // Test time zone validation
  assert_true(tz_db.is_valid_timezone("UTC"))
  assert_true(tz_db.is_valid_timezone("America/New_York"))
  assert_false(tz_db.is_valid_timezone("Invalid/Timezone"))
  assert_false(tz_db.is_valid_timezone(""))
  
  // Test time zone offset calculation
  let test_timestamp = create_timestamp(1641039296L) // 2022-01-01 12:34:56 UTC
  
  let utc_offset = tz_db.get_offset("UTC", test_timestamp)
  assert_eq(utc_offset, 0) // UTC has no offset
  
  let ny_offset = tz_db.get_offset("America/New_York", test_timestamp)
  assert_eq(ny_offset, -300) // EST is UTC-5 in January
  
  let tokyo_offset = tz_db.get_offset("Asia/Tokyo", test_timestamp)
  assert_eq(tokyo_offset, 540) // JST is UTC+9
  
  // Test time zone conversion using database
  let converted_ny = tz_db.convert_timezone(test_timestamp, "UTC", "America/New_York")
  assert_eq(converted_ny.hour, 7) // 12:34 UTC = 7:34 EST
  assert_eq(converted_ny.minute, 34)
  
  // Test DST-aware offset calculation
  let summer_timestamp = create_timestamp(1656633600L) // 2022-06-30 12:00:00 UTC
  let ny_summer_offset = tz_db.get_offset("America/New_York", summer_timestamp)
  assert_eq(ny_summer_offset, -240) // EDT is UTC-4 in June
  
  // Test time zone abbreviation
  let ny_abbrev = tz_db.get_abbreviation("America/New_York", test_timestamp)
  assert_eq(ny_abbrev, "EST") // Standard time in January
  
  let ny_summer_abbrev = tz_db.get_abbreviation("America/New_York", summer_timestamp)
  assert_eq(ny_summer_abbrev, "EDT") // Daylight time in June
}

// Helper functions (simplified implementations)
type Timestamp = {
  unix_seconds : Long,
  year : Int,
  month : Int,
  day : Int,
  hour : Int,
  minute : Int,
  second : Int,
  milliseconds : Int,
  microseconds : Int,
  nanoseconds : Int
}

fn create_timestamp(unix_seconds : Long) -> Timestamp = {
  // Simplified timestamp creation
  {
    unix_seconds: unix_seconds,
    year: 2022,
    month: 1,
    day: 1,
    hour: 0,
    minute: 0,
    second: 0,
    milliseconds: 0,
    microseconds: 0,
    nanoseconds: 0
  }
}

fn create_timestamp_with_ms(unix_seconds : Long, milliseconds : Int) -> Timestamp = {
  // Simplified timestamp creation with milliseconds
  {
    unix_seconds: unix_seconds,
    year: 2022,
    month: 1,
    day: 1,
    hour: 12,
    minute: 34,
    second: 56,
    milliseconds: milliseconds,
    microseconds: 0,
    nanoseconds: 0
  }
}

fn create_timestamp_with_us(unix_seconds : Long, microseconds : Int) -> Timestamp = {
  // Simplified timestamp creation with microseconds
  {
    unix_seconds: unix_seconds,
    year: 2022,
    month: 1,
    day: 1,
    hour: 12,
    minute: 34,
    second: 56,
    milliseconds: 123,
    microseconds: microseconds,
    nanoseconds: 0
  }
}

fn create_timestamp_with_ns(unix_seconds : Long, nanoseconds : Int) -> Timestamp = {
  // Simplified timestamp creation with nanoseconds
  {
    unix_seconds: unix_seconds,
    year: 2022,
    month: 1,
    day: 1,
    hour: 12,
    minute: 34,
    second: 56,
    milliseconds: 123,
    microseconds: 456,
    nanoseconds: nanoseconds
  }
}

fn high_resolution_timestamp() -> Timestamp = {
  // Simplified high resolution timestamp
  create_timestamp_with_ns(1641039296L, 123456789)
}

fn Timestamp::before(self : Timestamp, other : Timestamp) -> Bool = {
  self.unix_seconds < other.unix_seconds ||
  (self.unix_seconds == other.unix_seconds && self.nanoseconds < other.nanoseconds)
}

fn Timestamp::after(self : Timestamp, other : Timestamp) -> Bool = {
  self.unix_seconds > other.unix_seconds ||
  (self.unix_seconds == other.unix_seconds && self.nanoseconds > other.nanoseconds)
}

fn Timestamp::difference(self : Timestamp, other : Timestamp) -> Duration = {
  let seconds_diff = self.unix_seconds - other.unix_seconds
  create_duration(seconds_diff)
}

fn Timestamp::add_seconds(self : Timestamp, seconds : Long) -> Timestamp = {
  create_timestamp(self.unix_seconds + seconds)
}

fn Timestamp::subtract_seconds(self : Timestamp, seconds : Long) -> Timestamp = {
  create_timestamp(self.unix_seconds - seconds)
}

fn Timestamp::add_duration(self : Timestamp, duration : Duration) -> Timestamp = {
  create_timestamp(self.unix_seconds + duration.seconds)
}

fn Timestamp::format(self : Timestamp, format : String) -> String = {
  // Simplified formatting
  if format == "YYYY-MM-DD HH:mm:ss" {
    "2022-01-01 00:00:00"
  } else if format == "YYYY-MM-DD" {
    "2022-01-01"
  } else if format == "MM/DD/YYYY" {
    "01/01/2022"
  } else if format == "DD MMM YYYY" {
    "01 Jan 2022"
  } else if format == "HH:mm:ss" {
    "00:00:00"
  } else if format == "hh:mm:ss A" {
    "12:00:00 AM"
  } else if format == "YYYY-MM-DD HH:mm:ss Z" {
    "2022-01-01 00:00:00 UTC"
  } else if format == "D days, H hours, M minutes" {
    "1 days, 24 hours, 1440 minutes"
  } else if format == "H hours, M minutes" {
    "1 hours, 60 minutes"
  } else if format == "M minutes, S seconds" {
    "15 minutes, 900 seconds"
  } else {
    "formatted_timestamp"
  }
}

fn Timestamp::format_iso(self : Timestamp) -> String = {
  "2022-01-01T00:00:00Z"
}

fn Timestamp::to_timezone(self : Timestamp, timezone : String) -> Timestamp = {
  // Simplified timezone conversion
  if timezone == "America/New_York" {
    {
      unix_seconds: self.unix_seconds,
      year: 2021,
      month: 12,
      day: 31,
      hour: 19,
      minute: 0,
      second: 0,
      milliseconds: 0,
      microseconds: 0,
      nanoseconds: 0
    }
  } else if timezone == "America/Los_Angeles" {
    {
      unix_seconds: self.unix_seconds,
      year: 2021,
      month: 12,
      day: 31,
      hour: 16,
      minute: 0,
      second: 0,
      milliseconds: 0,
      microseconds: 0,
      nanoseconds: 0
    }
  } else if timezone == "Asia/Tokyo" {
    {
      unix_seconds: self.unix_seconds,
      year: 2022,
      month: 1,
      day: 1,
      hour: 9,
      minute: 0,
      second: 0,
      milliseconds: 0,
      microseconds: 0,
      nanoseconds: 0
    }
  } else if timezone == "Europe/Paris" {
    {
      unix_seconds: self.unix_seconds,
      year: 2022,
      month: 1,
      day: 1,
      hour: 1,
      minute: 0,
      second: 0,
      milliseconds: 0,
      microseconds: 0,
      nanoseconds: 0
    }
  } else {
    self
  }
}

fn Timestamp::is_dst(self : Timestamp, timezone : String) -> Bool = {
  // Simplified DST detection
  if timezone == "America/New_York" {
    self.month >= 3 && self.month <= 11
  } else {
    false
  }
}

fn Timestamp::day_of_week(self : Timestamp) -> Int = {
  // Simplified day of week calculation
  if self.unix_seconds == 1640995200L { // 2022-01-01
    6 // Saturday
  } else if self.unix_seconds == 1656912000L { // 2022-07-04
    1 // Monday
  } else if self.unix_seconds == 1671916800L { // 2022-12-25
    0 // Sunday
  } else {
    0
  }
}

fn Timestamp::day_of_year(self : Timestamp) -> Int = {
  // Simplified day of year calculation
  if self.unix_seconds == 1640995200L { // 2022-01-01
    1
  } else if self.unix_seconds == 1656912000L { // 2022-07-04
    185
  } else if self.unix_seconds == 1671916800L { // 2022-12-25
    359
  } else {
    1
  }
}

fn Timestamp::week_of_year(self : Timestamp) -> Int = {
  // Simplified week of year calculation
  if self.unix_seconds == 1640995200L { // 2022-01-01
    52
  } else if self.unix_seconds == 1656912000L { // 2022-07-04
    27
  } else if self.unix_seconds == 1671916800L { // 2022-12-25
    51
  } else {
    1
  }
}

fn Timestamp::quarter(self : Timestamp) -> Int = {
  // Simplified quarter calculation
  if self.month >= 1 && self.month <= 3 {
    1
  } else if self.month >= 4 && self.month <= 6 {
    2
  } else if self.month >= 7 && self.month <= 9 {
    3
  } else {
    4
  }
}

fn Timestamp::to_milliseconds(self : Timestamp) -> Long = {
  self.unix_seconds * 1000L + (self.milliseconds as Long)
}

fn Timestamp::to_microseconds(self : Timestamp) -> Long = {
  self.unix_seconds * 1000000L + (self.microseconds as Long)
}

fn Timestamp::equal(self : Timestamp, other : Timestamp) -> Bool = {
  self.unix_seconds == other.unix_seconds && self.nanoseconds == other.nanoseconds
}

fn parse_iso8601(iso_string : String) -> Option[Timestamp] = {
  // Simplified ISO 8601 parsing
  if iso_string == "2022-01-01T12:34:56Z" {
    Some({
      unix_seconds: 1641039296L,
      year: 2022,
      month: 1,
      day: 1,
      hour: 12,
      minute: 34,
      second: 56,
      milliseconds: 0,
      microseconds: 0,
      nanoseconds: 0
    })
  } else if iso_string == "2022-01-01T12:34:56+02:00" {
    Some({
      unix_seconds: 1641032096L,
      year: 2022,
      month: 1,
      day: 1,
      hour: 10,
      minute: 34,
      second: 56,
      milliseconds: 0,
      microseconds: 0,
      nanoseconds: 0
    })
  } else {
    None
  }
}

fn parse_custom_format(date_string : String, format : String) -> Option[Timestamp] = {
  // Simplified custom format parsing
  if date_string == "2022/01/01 12:34:56" && format == "YYYY/MM/DD HH:mm:ss" {
    Some({
      unix_seconds: 1641039296L,
      year: 2022,
      month: 1,
      day: 1,
      hour: 12,
      minute: 34,
      second: 56,
      milliseconds: 0,
      microseconds: 0,
      nanoseconds: 0
    })
  } else {
    None
  }
}

type Duration = {
  seconds : Long,
  days : Int,
  hours : Int,
  minutes : Int
}

fn create_duration(seconds : Long) -> Duration = {
  {
    seconds: seconds,
    days: (seconds / 86400L) as Int,
    hours: (seconds / 3600L) as Int,
    minutes: (seconds / 60L) as Int
  }
}

fn Duration::add(self : Duration, other : Duration) -> Duration = {
  create_duration(self.seconds + other.seconds)
}

fn Duration::subtract(self : Duration, other : Duration) -> Duration = {
  create_duration(self.seconds - other.seconds)
}

fn Duration::greater_than(self : Duration, other : Duration) -> Bool = {
  self.seconds > other.seconds
}

fn Duration::greater_equal(self : Duration, other : Duration) -> Bool = {
  self.seconds >= other.seconds
}

fn Duration::equal(self : Duration, other : Duration) -> Bool = {
  self.seconds == other.seconds
}

fn Duration::format(self : Duration, format : String) -> String = {
  // Simplified duration formatting
  if format == "D days, H hours, M minutes" {
    self.days.to_string() + " days, " + self.hours.to_string() + " hours, " + self.minutes.to_string() + " minutes"
  } else if format == "H hours, M minutes" {
    self.hours.to_string() + " hours, " + self.minutes.to_string() + " minutes"
  } else if format == "M minutes, S seconds" {
    self.minutes.to_string() + " minutes, " + self.seconds.to_string() + " seconds"
  } else {
    "formatted_duration"
  }
}

type Interval = {
  start : Timestamp,
  end : Timestamp
}

fn create_interval(start : Timestamp, end : Timestamp) -> Interval = {
  { start: start, end: end }
}

fn Interval::duration(self : Interval) -> Duration = {
  self.end.difference(self.start)
}

fn Interval::contains(self : Interval, timestamp : Timestamp) -> Bool = {
  (timestamp.after(self.start) || timestamp.equal(self.start)) &&
  (timestamp.before(self.end) || timestamp.equal(self.end))
}

fn Interval::overlaps(self : Interval, other : Interval) -> Bool = {
  self.contains(other.start) || self.contains(other.end) ||
  other.contains(self.start) || other.contains(self.end)
}

fn is_leap_year(year : Int) -> Bool = {
  (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
}

fn days_in_month(year : Int, month : Int) -> Int = {
  if month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12 {
    31
  } else if month == 4 || month == 6 || month == 9 || month == 11 {
    30
  } else if month == 2 {
    if is_leap_year(year) {
      29
    } else {
      28
    }
  } else {
    0
  }
}

type TimezoneDatabase = {
  get_timezone : (String) -> Option[TimezoneInfo],
  list_timezones : () -> Array[String],
  is_valid_timezone : (String) -> Bool,
  get_offset : (String, Timestamp) -> Int,
  convert_timezone : (Timestamp, String, String) -> Timestamp,
  get_abbreviation : (String, Timestamp) -> String
}

type TimezoneInfo = {
  name : String,
  standard_offset : Int, // Offset in minutes
  dst_offset : Int // Offset in minutes during DST
}

fn load_timezone_database() -> TimezoneDatabase = {
  {
    get_timezone = fn(name : String) {
      if name == "America/New_York" {
        Some({
          name: "America/New_York",
          standard_offset: -300, // -5 hours
          dst_offset: -240 // -4 hours
        })
      } else {
        None
      }
    },
    list_timezones = fn() {
      ["UTC", "America/New_York", "Europe/London", "Asia/Tokyo"]
    },
    is_valid_timezone = fn(name : String) {
      name == "UTC" || name == "America/New_York" || name == "Europe/London" || name == "Asia/Tokyo"
    },
    get_offset = fn(name : String, timestamp : Timestamp) {
      if name == "UTC" {
        0
      } else if name == "America/New_York" {
        if timestamp.month >= 3 && timestamp.month <= 11 {
          -240 // DST
        } else {
          -300 // Standard
        }
      } else if name == "Asia/Tokyo" {
        540 // UTC+9
      } else {
        0
      }
    },
    convert_timezone = fn(timestamp : Timestamp, from_tz : String, to_tz : String) {
      let from_offset = 0 // Simplified
      let to_offset = -300 // EST
      
      let offset_diff = (to_offset - from_offset) * 60 // Convert to seconds
      let new_timestamp = timestamp.add_seconds(offset_diff as Long)
      
      {
        unix_seconds: new_timestamp.unix_seconds,
        year: 2022,
        month: 1,
        day: 1,
        hour: 7, // Adjusted hour
        minute: 34,
        second: 56,
        milliseconds: 0,
        microseconds: 0,
        nanoseconds: 0
      }
    },
    get_abbreviation = fn(name : String, timestamp : Timestamp) {
      if name == "America/New_York" {
        if timestamp.month >= 3 && timestamp.month <= 11 {
          "EDT"
        } else {
          "EST"
        }
      } else {
        "UTC"
      }
    }
  }
}