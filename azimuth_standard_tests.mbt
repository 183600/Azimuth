// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(6, @azimuth.subtract(10, 4))
  @azimuth.assert_eq(4, @azimuth.divide(20, 5))
}

test "string_concatenation" {
  // 字符串连接测试
  @azimuth.assert_eq_string("Hello World", @azimuth.concat("Hello", " World"))
  @azimuth.assert_eq_string("MoonBit", @azimuth.concat("Moon", "Bit"))
  @azimuth.assert_eq_string("123456", @azimuth.concat("123", "456"))
}

test "boolean_operations" {
  // 布尔运算测试
  @azimuth.assert_eq(true, @azimuth.and(true, true))
  @azimuth.assert_eq(false, @azimuth.and(true, false))
  @azimuth.assert_eq(true, @azimuth.or(true, false))
  @azimuth.assert_eq(false, @azimuth.or(false, false))
  @azimuth.assert_eq(false, @azimuth.not(true))
  @azimuth.assert_eq(true, @azimuth.not(false))
}

test "comparison_operations" {
  // 比较运算测试
  @azimuth.assert_eq(true, @azimuth.greater_than(10, 5))
  @azimuth.assert_eq(false, @azimuth.less_than(10, 5))
  @azimuth.assert_eq(true, @azimuth.equal(5, 5))
  @azimuth.assert_eq(false, @azimuth.not_equal(5, 5))
  @azimuth.assert_eq(true, @azimuth.greater_or_equal(10, 10))
  @azimuth.assert_eq(true, @azimuth.less_or_equal(5, 10))
}

test "mathematical_functions" {
  // 数学函数测试
  @azimuth.assert_eq(0, @azimuth.abs(0))
  @azimuth.assert_eq(5, @azimuth.abs(-5))
  @azimuth.assert_eq(5, @azimuth.abs(5))
  @azimuth.assert_eq(16, @azimuth.square(4))
  @azimuth.assert_eq(27, @azimuth.cube(3))
  @azimuth.assert_eq(0, @azimuth.factorial(0))
  @azimuth.assert_eq(1, @azimuth.factorial(1))
}

test "array_operations" {
  // 数组操作测试
  let array1 = @azimuth.create_array(3)
  @azimuth.assert_eq(3, @azimuth.array_length(array1))
  
  let array2 = @azimuth.array_push(array1, 42)
  @azimuth.assert_eq(4, @azimuth.array_length(array2))
  
  let value = @azimuth.array_get(array2, 3)
  @azimuth.assert_eq(42, value)
}

test "date_time_operations" {
  // 日期时间操作测试
  let date = @azimuth.create_date(2026, 1, 17)
  @azimuth.assert_eq(2026, @azimuth.get_year(date))
  @azimuth.assert_eq(1, @azimuth.get_month(date))
  @azimuth.assert_eq(17, @azimuth.get_day(date))
  
  let next_day = @azimuth.add_days(date, 1)
  @azimuth.assert_eq(18, @azimuth.get_day(next_day))
}

test "error_handling" {
  // 错误处理测试
  let result1 = @azimuth.safe_divide(10, 2)
  @azimuth.assert_eq(true, @azimuth.is_success(result1))
  @azimuth.assert_eq(5, @azimuth.get_value(result1))
  
  let result2 = @azimuth.safe_divide(10, 0)
  @azimuth.assert_eq(false, @azimuth.is_success(result2))
  @azimuth.assert_eq("Division by zero", @azimuth.get_error(result2))
}

test "type_conversions" {
  // 类型转换测试
  @azimuth.assert_eq_string("123", @azimuth.int_to_string(123))
  @azimuth.assert_eq(123, @azimuth.string_to_int("123"))
  @azimuth.assert_eq_string("3.14", @azimuth.float_to_string(3.14))
  @azimuth.assert_eq(3.14, @azimuth.string_to_float("3.14"))
}

test "complex_calculations" {
  // 复杂计算测试
  // 计算二次方程的判别式: b² - 4ac
  let a = 1
  let b = 5
  let c = 6
  let discriminant = @azimuth.subtract(@azimuth.square(b), @azimuth.multiply(@azimuth.multiply(4, a), c))
  @azimuth.assert_eq(1, discriminant)
  
  // 计算圆的面积: πr²
  let radius = 5
  let area = @azimuth.multiply(3.14159, @azimuth.square(radius))
  @azimuth.assert_eq(78.53975, area)
}