// Azimuth Telemetry System - Time and Date Operations Tests
// This file contains test cases for time and date operations used in the telemetry system

// Test 1: Basic Time Operations
test "basic time operations" {
  // Test current time retrieval
  let current_time = TimeUtil::current_time()
  assert_true(current_time > 0L)
  
  // Test time conversion to seconds
  let time_in_seconds = TimeUtil::to_seconds(current_time)
  assert_true(time_in_seconds > 0L)
  
  // Test time conversion to milliseconds
  let time_in_millis = TimeUtil::to_milliseconds(current_time)
  assert_true(time_in_millis > 0L)
  
  // Test time conversion to nanoseconds
  let time_in_nanos = TimeUtil::to_nanoseconds(current_time)
  assert_true(time_in_nanos > 0L)
  
  // Test time arithmetic
  let later_time = current_time + 5000L // 5 seconds later
  let time_diff = TimeUtil::difference(later_time, current_time)
  assert_eq(time_diff, 5000L)
  
  // Test time comparison
  assert_true(TimeUtil::is_after(later_time, current_time))
  assert_false(TimeUtil::is_after(current_time, later_time))
  assert_true(TimeUtil::is_before(current_time, later_time))
  assert_false(TimeUtil::is_before(later_time, current_time))
}

// Test 2: Time Formatting and Parsing
test "time formatting and parsing" {
  let timestamp = 1609459200L // 2021-01-01 00:00:00 UTC
  
  // Test ISO 8601 formatting
  let iso_formatted = TimeUtil::format_iso8601(timestamp)
  assert_eq(iso_formatted, "2021-01-01T00:00:00Z")
  
  // Test RFC 3339 formatting
  let rfc_formatted = TimeUtil::format_rfc3339(timestamp)
  assert_eq(rfc_formatted, "2021-01-01T00:00:00Z")
  
  // Test custom formatting
  let custom_formatted = TimeUtil::format_custom(timestamp, "yyyy-MM-dd HH:mm:ss")
  assert_eq(custom_formatted, "2021-01-01 00:00:00")
  
  // Test parsing ISO 8601
  let parsed_iso = TimeUtil::parse_iso8601("2021-01-01T00:00:00Z")
  match parsed_iso {
    Some(value) => assert_eq(value, timestamp)
    None => assert_true(false)
  }
  
  // Test parsing RFC 3339
  let parsed_rfc = TimeUtil::parse_rfc3339("2021-01-01T00:00:00Z")
  match parsed_rfc {
    Some(value) => assert_eq(value, timestamp)
    None => assert_true(false)
  }
  
  // Test parsing custom format
  let parsed_custom = TimeUtil::parse_custom("2021-01-01 00:00:00", "yyyy-MM-dd HH:mm:ss")
  match parsed_custom {
    Some(value) => assert_eq(value, timestamp)
    None => assert_true(false)
  }
  
  // Test parsing invalid format
  let invalid_parse = TimeUtil::parse_iso8601("not-a-timestamp")
  match invalid_parse {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 3: Date Operations
test "date operations" {
  let base_date = DateUtil::create(2021, 1, 1)
  
  // Test date components
  assert_eq(DateUtil::year(base_date), 2021)
  assert_eq(DateUtil::month(base_date), 1)
  assert_eq(DateUtil::day(base_date), 1)
  
  // Test day of week
  assert_eq(DateUtil::day_of_week(base_date), 5) // Friday
  
  // Test day of year
  assert_eq(DateUtil::day_of_year(base_date), 1)
  
  // Test week of year
  assert_eq(DateUtil::week_of_year(base_date), 53) // Last week of previous year
  
  // Test date arithmetic - add days
  let next_day = DateUtil::add_days(base_date, 1)
  assert_eq(DateUtil::year(next_day), 2021)
  assert_eq(DateUtil::month(next_day), 1)
  assert_eq(DateUtil::day(next_day), 2)
  
  // Test date arithmetic - add months
  let next_month = DateUtil::add_months(base_date, 1)
  assert_eq(DateUtil::year(next_month), 2021)
  assert_eq(DateUtil::month(next_month), 2)
  assert_eq(DateUtil::day(next_month), 1)
  
  // Test date arithmetic - add years
  let next_year = DateUtil::add_years(base_date, 1)
  assert_eq(DateUtil::year(next_year), 2022)
  assert_eq(DateUtil::month(next_year), 1)
  assert_eq(DateUtil::day(next_year), 1)
  
  // Test date comparison
  assert_true(DateUtil::is_after(next_day, base_date))
  assert_false(DateUtil::is_after(base_date, next_day))
  assert_true(DateUtil::is_before(base_date, next_day))
  assert_false(DateUtil::is_before(next_day, base_date))
  assert_true(DateUtil::equals(base_date, DateUtil::create(2021, 1, 1)))
  assert_false(DateUtil::equals(base_date, next_day))
  
  // Test date difference
  let days_diff = DateUtil::days_between(next_day, base_date)
  assert_eq(days_diff, 1)
  
  let months_diff = DateUtil::months_between(next_month, base_date)
  assert_eq(months_diff, 1)
  
  let years_diff = DateUtil::years_between(next_year, base_date)
  assert_eq(years_diff, 1)
}

// Test 4: Time Zone Operations
test "time zone operations" {
  let utc_time = 1609459200L // 2021-01-01 00:00:00 UTC
  
  // Test time zone conversion
  let est_time = TimeZoneUtil::convert(utc_time, "America/New_York")
  // EST is UTC-5 in winter
  let expected_est = utc_time - (5 * 3600L)
  assert_eq(est_time, expected_est)
  
  let pst_time = TimeZoneUtil::convert(utc_time, "America/Los_Angeles")
  // PST is UTC-8 in winter
  let expected_pst = utc_time - (8 * 3600L)
  assert_eq(pst_time, expected_pst)
  
  // Test time zone offset
  let est_offset = TimeZoneUtil::get_offset("America/New_York", utc_time)
  assert_eq(est_offset, -5 * 3600L)
  
  let pst_offset = TimeZoneUtil::get_offset("America/Los_Angeles", utc_time)
  assert_eq(pst_offset, -8 * 3600L)
  
  // Test time zone abbreviation
  let est_abbr = TimeZoneUtil::get_abbreviation("America/New_York", utc_time)
  assert_eq(est_abbr, "EST")
  
  let pst_abbr = TimeZoneUtil::get_abbreviation("America/Los_Angeles", utc_time)
  assert_eq(pst_abbr, "PST")
  
  // Test invalid time zone
  let invalid_tz = TimeZoneUtil::convert(utc_time, "Invalid/Timezone")
  assert_eq(invalid_tz, utc_time) // Should return original time
}

// Test 5: Duration Operations
test "duration operations" {
  // Test duration creation
  let duration1 = DurationUtil::from_seconds(3600) // 1 hour
  let duration2 = DurationUtil::from_minutes(120)   // 2 hours
  let duration3 = DurationUtil::from_hours(1)       // 1 hour
  
  // Test duration conversion
  assert_eq(DurationUtil::to_seconds(duration1), 3600L)
  assert_eq(DurationUtil::to_minutes(duration1), 60L)
  assert_eq(DurationUtil::to_hours(duration1), 1L)
  
  // Test duration arithmetic
  let sum_duration = DurationUtil::add(duration1, duration2)
  assert_eq(DurationUtil::to_hours(sum_duration), 3L)
  
  let diff_duration = DurationUtil::subtract(duration2, duration1)
  assert_eq(DurationUtil::to_hours(diff_duration), 1L)
  
  // Test duration comparison
  assert_true(DurationUtil::equals(duration1, duration3))
  assert_false(DurationUtil::equals(duration1, duration2))
  assert_true(DurationUtil::is_greater(duration2, duration1))
  assert_false(DurationUtil::is_greater(duration1, duration2))
  
  // Test duration formatting
  let formatted_duration = DurationUtil::format(duration1)
  assert_eq(formatted_duration, "1h 0m 0s")
  
  let long_duration = DurationUtil::from_seconds(3661) // 1 hour, 1 minute, 1 second
  let formatted_long = DurationUtil::format(long_duration)
  assert_eq(formatted_long, "1h 1m 1s")
}

// Test 6: Time Interval Operations
test "time interval operations" {
  let start_time = 1609459200L // 2021-01-01 00:00:00 UTC
  let end_time = 1609545600L   // 2021-01-02 00:00:00 UTC
  
  // Test interval creation
  let interval = TimeIntervalUtil::create(start_time, end_time)
  
  // Test interval properties
  assert_eq(TimeIntervalUtil::start_time(interval), start_time)
  assert_eq(TimeIntervalUtil::end_time(interval), end_time)
  assert_eq(TimeIntervalUtil::duration(interval), end_time - start_time)
  
  // Test interval contains
  assert_true(TimeIntervalUtil::contains(interval, start_time + 3600L))
  assert_true(TimeIntervalUtil::contains(interval, end_time - 3600L))
  assert_false(TimeIntervalUtil::contains(interval, start_time - 3600L))
  assert_false(TimeIntervalUtil::contains(interval, end_time + 3600L))
  
  // Test interval overlap
  let overlapping_interval = TimeIntervalUtil::create(
    start_time + 3600L, 
    end_time - 3600L
  )
  assert_true(TimeIntervalUtil::overlaps(interval, overlapping_interval))
  
  let non_overlapping_interval = TimeIntervalUtil::create(
    end_time + 3600L, 
    end_time + 7200L
  )
  assert_false(TimeIntervalUtil::overlaps(interval, non_overlapping_interval))
  
  // Test interval intersection
  let intersection = TimeIntervalUtil::intersection(interval, overlapping_interval)
  match intersection {
    Some(inter) => {
      assert_eq(TimeIntervalUtil::start_time(inter), start_time + 3600L)
      assert_eq(TimeIntervalUtil::end_time(inter), end_time - 3600L)
    }
    None => assert_true(false)
  }
  
  let no_intersection = TimeIntervalUtil::intersection(interval, non_overlapping_interval)
  match no_intersection {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test interval union
  let union = TimeIntervalUtil::union(interval, overlapping_interval)
  match union {
    Some(u) => {
      assert_eq(TimeIntervalUtil::start_time(u), start_time)
      assert_eq(TimeIntervalUtil::end_time(u), end_time)
    }
    None => assert_true(false)
  }
}

// Test 7: Timer and Stopwatch Operations
test "timer and stopwatch operations" {
  // Test stopwatch
  let stopwatch = StopwatchUtil::create()
  
  // Test initial state
  assert_false(StopwatchUtil::is_running(stopwatch))
  assert_eq(StopwatchUtil::elapsed(stopwatch), 0L)
  
  // Test start
  StopwatchUtil::start(stopwatch)
  assert_true(StopwatchUtil::is_running(stopwatch))
  
  // Simulate some time passing
  // In a real test, we would wait, but for unit tests we'll mock this
  StopwatchUtil::advance(stopwatch, 1000L) // Advance 1 second
  
  // Test elapsed time
  let elapsed = StopwatchUtil::elapsed(stopwatch)
  assert_true(elapsed >= 1000L)
  
  // Test stop
  StopwatchUtil::stop(stopwatch)
  assert_false(StopwatchUtil::is_running(stopwatch))
  
  let stopped_elapsed = StopwatchUtil::elapsed(stopwatch)
  assert_true(stopped_elapsed >= 1000L)
  
  // Test reset
  StopwatchUtil::reset(stopwatch)
  assert_false(StopwatchUtil::is_running(stopwatch))
  assert_eq(StopwatchUtil::elapsed(stopwatch), 0L)
  
  // Test timer
  let timer = TimerUtil::create(5000L) // 5 second timer
  
  // Test initial state
  assert_false(TimerUtil::is_expired(timer))
  assert_false(TimerUtil::is_running(timer))
  
  // Test start
  TimerUtil::start(timer)
  assert_true(TimerUtil::is_running(timer))
  assert_false(TimerUtil::is_expired(timer))
  
  // Simulate time passing
  TimerUtil::advance(timer, 3000L) // Advance 3 seconds
  assert_false(TimerUtil::is_expired(timer))
  
  TimerUtil::advance(timer, 3000L) // Advance 3 more seconds (total 6)
  assert_true(TimerUtil::is_expired(timer))
  
  // Test remaining time
  let remaining = TimerUtil::remaining(timer)
  assert_true(remaining <= 0L)
  
  // Test reset
  TimerUtil::reset(timer)
  assert_false(TimerUtil::is_expired(timer))
  assert_false(TimerUtil::is_running(timer))
}