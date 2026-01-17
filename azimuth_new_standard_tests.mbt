// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，包含断言验证

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(10, @azimuth.add(@azimuth.multiply(2, 3), 4))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "string_concatenation" {
  // 字符串连接测试
  @azimuth.assert_eq_string("Hello World", @azimuth.concat("Hello", " World"))
  @azimuth.assert_eq_string("MoonBit", @azimuth.concat("Moon", "Bit"))
  @azimuth.assert_eq_string("123456", @azimuth.concat("123", "456"))
  @azimuth.assert_eq_string("", @azimuth.concat("", ""))
}

test "array_operations" {
  // 数组操作测试
  let arr1 = [1, 2, 3, 4, 5]
  let arr2 = [6, 7, 8, 9, 10]
  
  // 数组长度测试
  @azimuth.assert_eq(5, @azimuth.length(arr1))
  @azimuth.assert_eq(5, @azimuth.length(arr2))
  
  // 数组元素访问测试
  @azimuth.assert_eq(1, @azimuth.get(arr1, 0))
  @azimuth.assert_eq(3, @azimuth.get(arr1, 2))
  @azimuth.assert_eq(10, @azimuth.get(arr2, 4))
}

test "boolean_logic" {
  // 布尔逻辑测试
  @azimuth.assert_eq(true, @azimuth.and(true, true))
  @azimuth.assert_eq(false, @azimuth.and(true, false))
  @azimuth.assert_eq(false, @azimuth.and(false, true))
  @azimuth.assert_eq(false, @azimuth.and(false, false))
  
  @azimuth.assert_eq(true, @azimuth.or(true, false))
  @azimuth.assert_eq(true, @azimuth.or(false, true))
  @azimuth.assert_eq(true, @azimuth.or(true, true))
  @azimuth.assert_eq(false, @azimuth.or(false, false))
  
  @azimuth.assert_eq(false, @azimuth.not(true))
  @azimuth.assert_eq(true, @azimuth.not(false))
}

test "comparison_operations" {
  // 比较运算测试
  @azimuth.assert_eq(true, @azimuth.gt(5, 3))
  @azimuth.assert_eq(false, @azimuth.gt(3, 5))
  @azimuth.assert_eq(false, @azimuth.gt(5, 5))
  
  @azimuth.assert_eq(true, @azimuth.lt(3, 5))
  @azimuth.assert_eq(false, @azimuth.lt(5, 3))
  @azimuth.assert_eq(false, @azimuth.lt(5, 5))
  
  @azimuth.assert_eq(true, @azimuth.eq(5, 5))
  @azimuth.assert_eq(false, @azimuth.eq(5, 3))
  
  @azimuth.assert_eq(true, @azimuth.ge(5, 3))
  @azimuth.assert_eq(true, @azimuth.ge(5, 5))
  @azimuth.assert_eq(false, @azimuth.ge(3, 5))
  
  @azimuth.assert_eq(true, @azimuth.le(3, 5))
  @azimuth.assert_eq(true, @azimuth.le(5, 5))
  @azimuth.assert_eq(false, @azimuth.le(5, 3))
}

test "mathematical_functions" {
  // 数学函数测试
  // 绝对值测试
  @azimuth.assert_eq(5, @azimuth.abs(5))
  @azimuth.assert_eq(5, @azimuth.abs(-5))
  @azimuth.assert_eq(0, @azimuth.abs(0))
  
  // 平方测试
  @azimuth.assert_eq(4, @azimuth.square(2))
  @azimuth.assert_eq(9, @azimuth.square(3))
  @azimuth.assert_eq(16, @azimuth.square(4))
  @azimuth.assert_eq(0, @azimuth.square(0))
  
  // 平方根测试
  @azimuth.assert_eq(2, @azimuth.sqrt(4))
  @azimuth.assert_eq(3, @azimuth.sqrt(9))
  @azimuth.assert_eq(4, @azimuth.sqrt(16))
  @azimuth.assert_eq(0, @azimuth.sqrt(0))
}

test "error_handling" {
  // 错误处理测试
  let result1 = @azimuth.divide(10, 2)
  let result2 = @azimuth.divide(10, 5)
  let result3 = @azimuth.divide(0, 5)
  
  @azimuth.assert_eq(5, result1)
  @azimuth.assert_eq(2, result2)
  @azimuth.assert_eq(0, result3)
  
  // 除零错误处理
  let error_result = @azimuth.divide(10, 0)
  @azimuth.assert_eq(0, error_result)  // 假设除零返回0
}

test "data_type_conversions" {
  // 数据类型转换测试
  // 字符串到整数转换
  @azimuth.assert_eq(123, @azimuth.to_int("123"))
  @azimuth.assert_eq(0, @azimuth.to_int("0"))
  @azimuth.assert_eq(-456, @azimuth.to_int("-456"))
  
  // 整数到字符串转换
  @azimuth.assert_eq_string("123", @azimuth.to_string(123))
  @azimuth.assert_eq_string("0", @azimuth.to_string(0))
  @azimuth.assert_eq_string("-456", @azimuth.to_string(-456))
  
  // 布尔值转换
  @azimuth.assert_eq_string("true", @azimuth.bool_to_string(true))
  @azimuth.assert_eq_string("false", @azimuth.bool_to_string(false))
}

test "complex_calculations" {
  // 复杂计算测试
  // 计算圆的面积: π * r²
  let radius = 5
  let pi = 3.14159
  let circle_area = @azimuth.multiply(pi, @azimuth.square(radius))
  @azimuth.assert_eq(78.53975, circle_area)
  
  // 计算球的体积: (4/3) * π * r³
  let sphere_volume = @azimuth.multiply(@azimuth.multiply(4.0, pi) / 3.0, @azimuth.cube(radius))
  @azimuth.assert_eq(523.59833, sphere_volume)
  
  // 计算二次方程: ax² + bx + c
  let a = 2
  let b = 3
  let c = 1
  let x = 2
  let quadratic_result = @azimuth.add(@azimuth.add(@azimuth.multiply(a, @azimuth.square(x)), @azimuth.multiply(b, x)), c)
  @azimuth.assert_eq(15, quadratic_result)
}

test "real_world_scenarios" {
  // 真实世界场景测试
  // 计算购物车总价
  let item1_price = 29.99
  let item2_price = 19.99
  let item3_price = 49.99
  let tax_rate = 0.08  // 8% 税率
  
  let subtotal = @azimuth.add(@azimuth.add(item1_price, item2_price), item3_price)
  let tax = @azimuth.multiply(subtotal, tax_rate)
  let total = @azimuth.add(subtotal, tax)
  
  @azimuth.assert_eq(99.97, subtotal)
  @azimuth.assert_eq(7.9976, tax)
  @azimuth.assert_eq(107.9676, total)
  
  // 计算平均分
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 96
  let score5 = 88
  
  let sum_scores = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4), score5)
  let average = @azimuth.divide(sum_scores, 5)
  
  @azimuth.assert_eq(439, sum_scores)
  @azimuth.assert_eq(87.8, average)
}