// 附加 MoonBit 测试用例

test "power_calculation" {
  let base = 3
  let step1 = @azimuth.multiply(base, base)
  let step2 = @azimuth.multiply(step1, base)
  let result = @azimuth.multiply(step2, base)
  @azimuth.assert_eq(result, 81)
}

test "factorial_calculation" {
  let step1 = @azimuth.multiply(5, 4)
  let step2 = @azimuth.multiply(step1, 3)
  let step3 = @azimuth.multiply(step2, 2)
  let result = @azimuth.multiply(step3, 1)
  @azimuth.assert_eq(result, 120)
}

test "compound_interest_calculation" {
  let principal = 1000
  let rate = 5
  let year1_interest = @azimuth.multiply(principal, rate) / 100
  let year1_total = @azimuth.add(principal, year1_interest)
  let year2_interest = @azimuth.multiply(year1_total, rate) / 100
  let year2_total = @azimuth.add(year1_total, year2_interest)
  let year3_interest = @azimuth.multiply(year2_total, rate) / 100
  let final_total = @azimuth.add(year2_total, year3_interest)
  @azimuth.assert_eq(final_total, 1157)
}

test "string_length_effects" {
  let empty = ""
  let short = "Hi"
  let medium = "MoonBit"
  @azimuth.assert_eq_string(@azimuth.greet(empty), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet(short), "Hello, Hi!")
  @azimuth.assert_eq_string(@azimuth.greet(medium), "Hello, MoonBit!")
}

test "statistical_operations" {
  let num1 = 10
  let num2 = 20
  let num3 = 30
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), 40)
  let total = @azimuth.add(sum, 50)
  @azimuth.assert_eq(total, 150)
  let average = total / 5
  @azimuth.assert_eq(average, 30)
}

test "geometric_calculations" {
  let radius = 5
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(circumference, 30)
  let radius_squared = @azimuth.multiply(radius, radius)
  let area = @azimuth.multiply(3, radius_squared)
  @azimuth.assert_eq(area, 75)
}

test "time_calculations" {
  let total_seconds = 3661
  let hours = total_seconds / 3600
  @azimuth.assert_eq(hours, 1)
  let remaining_seconds = @azimuth.add(total_seconds, -hours * 3600)
  @azimuth.assert_eq(remaining_seconds, 61)
  let minutes = remaining_seconds / 60
  @azimuth.assert_eq(minutes, 1)
  let final_seconds = @azimuth.add(remaining_seconds, -minutes * 60)
  @azimuth.assert_eq(final_seconds, 1)
}

test "currency_conversion" {
  let usd_amount = 100
  let exchange_rate = 7
  let cny_amount = @azimuth.multiply(usd_amount, exchange_rate)
  @azimuth.assert_eq(cny_amount, 700)
  let converted_back = cny_amount / exchange_rate
  @azimuth.assert_eq(converted_back, usd_amount)
}

test "temperature_conversion_detailed" {
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(fahrenheit, 80)
  let kelvin = @azimuth.add(celsius, 273)
  @azimuth.assert_eq(kelvin, 298)
  let converted_celsius = @azimuth.add(fahrenheit, -30) / 2
  @azimuth.assert_eq(converted_celsius, celsius)
}

test "max_min_value_search" {
  let value1 = 15
  let value2 = 42
  let value3 = 7
  let value4 = 89
  let value5 = 23
  
  let max1 = if value1 > value2 { value1 } else { value2 }
  let max2 = if max1 > value3 { max1 } else { value3 }
  let max3 = if max2 > value4 { max2 } else { value4 }
  let max_value = if max3 > value5 { max3 } else { value5 }
  
  @azimuth.assert_eq(max_value, 89)
  
  let min1 = if value1 < value2 { value1 } else { value2 }
  let min2 = if min1 < value3 { min1 } else { value3 }
  let min3 = if min2 < value4 { min2 } else { value4 }
  let min_value = if min3 < value5 { min3 } else { value5 }
  
  @azimuth.assert_eq(min_value, 7)
}