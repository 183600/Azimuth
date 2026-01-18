// Azimuth 项目标准测试用例集
// 使用标准 MoonBit 测试语法，包含实用的测试场景

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(15, @azimuth.add(@azimuth.multiply(4, 3), 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "string_concatenation" {
  // 字符串连接测试
  @azimuth.assert_eq_string("Hello World", @azimuth.add("Hello", " World"))
  @azimuth.assert_eq_string("MoonBit", @azimuth.add("Moon", "Bit"))
  @azimuth.assert_eq_string("12345", @azimuth.add("123", "45"))
}

test "calculation_with_variables" {
  // 使用变量的计算测试
  let x = 10
  let y = 20
  let z = 5
  
  @azimuth.assert_eq(30, @azimuth.add(x, y))
  @azimuth.assert_eq(200, @azimuth.multiply(x, y))
  @azimuth.assert_eq(15, @azimuth.add(x, z))
  @azimuth.assert_eq(50, @azimuth.multiply(y, z))
}

test "nested_expressions" {
  // 嵌套表达式测试
  @azimuth.assert_eq(14, @azimuth.add(@azimuth.multiply(2, 5), 4))
  @azimuth.assert_eq(100, @azimuth.multiply(@azimuth.add(5, 5), 10))
  @azimuth.assert_eq(32, @azimuth.add(@azimuth.multiply(@azimuth.add(2, 2), 8), 0))
}

test "sequence_calculations" {
  // 序列计算测试
  let a = 1
  let b = @azimuth.add(a, 1)  // 2
  let c = @azimuth.add(b, 1)  // 3
  let d = @azimuth.add(c, 1)  // 4
  let e = @azimuth.add(d, 1)  // 5
  
  @azimuth.assert_eq(3, c)
  @azimuth.assert_eq(5, e)
  @azimuth.assert_eq(15, @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(a, b), c), d), e))
}

test "business_calculations" {
  // 商业计算测试
  let unit_price = 25
  let quantity = 4
  let tax_rate = 10
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  let total = @azimuth.add(subtotal, tax)
  
  @azimuth.assert_eq(100, subtotal)
  @azimuth.assert_eq(10, tax)
  @azimuth.assert_eq(110, total)
}

test "geometry_formulas" {
  // 几何公式测试
  let radius = 5
  
  // 圆的周长 (近似值)
  let circumference = @azimuth.multiply(2, @azimuth.multiply(radius, 3))
  @azimuth.assert_eq(30, circumference)
  
  // 正方形面积
  let side = 6
  let area = @azimuth.multiply(side, side)
  @azimuth.assert_eq(36, area)
}

test "time_conversions" {
  // 时间转换测试
  let hours = 2
  let minutes = 30
  
  // 转换为总分钟数
  let total_minutes = @azimuth.add(@azimuth.multiply(hours, 60), minutes)
  @azimuth.assert_eq(150, total_minutes)
  
  // 转换为总秒数
  let total_seconds = @azimuth.multiply(total_minutes, 60)
  @azimuth.assert_eq(9000, total_seconds)
}

test "array_index_simulation" {
  // 数组索引模拟测试
  let base = 100
  let index1 = 0
  let index2 = 1
  let index3 = 2
  
  let value1 = @azimuth.add(base, @azimuth.multiply(index1, 10))
  let value2 = @azimuth.add(base, @azimuth.multiply(index2, 10))
  let value3 = @azimuth.add(base, @azimuth.multiply(index3, 10))
  
  @azimuth.assert_eq(100, value1)
  @azimuth.assert_eq(110, value2)
  @azimuth.assert_eq(120, value3)
}