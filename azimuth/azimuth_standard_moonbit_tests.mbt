// Azimuth 项目的标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法编写

// 导入被测试的模块
use azimuth

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  azimuth.assert_eq(10, azimuth.add(3, 7))
  azimuth.assert_eq(0, azimuth.add(-5, 5))
  azimuth.assert_eq(-8, azimuth.add(-3, -5))
  
  azimuth.assert_eq(15, azimuth.multiply(3, 5))
  azimuth.assert_eq(0, azimuth.multiply(10, 0))
  azimuth.assert_eq(-12, azimuth.multiply(-3, 4))
  
  azimuth.assert_eq(5, azimuth.subtract(12, 7))
  azimuth.assert_eq(-5, azimuth.subtract(3, 8))
  azimuth.assert_eq(0, azimuth.subtract(10, 10))
}

test "division_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  azimuth.assert_eq(4, azimuth.divide_with_ceil(10, 3))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(9, 3))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(5, 5))
  azimuth.assert_eq(2, azimuth.divide_with_ceil(7, 4))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 10))
}

test "division_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  azimuth.assert_eq(0, azimuth.divide_with_ceil(5, 0))  // 除数为0的情况
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 1))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(-1, -1))
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  azimuth.assert_eq(-4, azimuth.divide_with_ceil(10, -3))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  azimuth.assert_eq_string("Hello, World!", azimuth.greet("World"))
  azimuth.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  azimuth.assert_eq_string("Hello, Alice!", azimuth.greet("Alice"))
  azimuth.assert_eq_string("Hello, 123!", azimuth.greet("123"))
}

test "mathematical_expressions" {
  // 测试数学表达式组合
  let a = 5
  let b = 3
  let c = 2
  
  // 复合运算
  let result1 = azimuth.add(azimuth.multiply(a, b), c)  // 5*3 + 2 = 17
  let result2 = azimuth.multiply(azimuth.add(a, b), c)  // (5+3)*2 = 16
  let result3 = azimuth.subtract(azimuth.multiply(a, b), c)  // 5*3 - 2 = 13
  
  azimuth.assert_eq(17, result1)
  azimuth.assert_eq(16, result2)
  azimuth.assert_eq(13, result3)
}

test "boundary_value_testing" {
  // 测试边界值
  azimuth.assert_eq(2147483647, azimuth.add(2147483646, 1))  // 接近最大值
  azimuth.assert_eq(-2147483648, azimuth.subtract(-2147483647, 1))  // 接近最小值
  azimuth.assert_eq(1, azimuth.divide_with_ceil(2147483647, 2147483647))
  azimuth.assert_eq(2147483647, azimuth.multiply(1, 2147483647))
}

test "negative_number_operations" {
  // 测试负数运算
  azimuth.assert_eq(-8, azimuth.add(-3, -5))
  azimuth.assert_eq(2, azimuth.add(-5, 7))
  azimuth.assert_eq(15, azimuth.multiply(-3, -5))
  azimuth.assert_eq(-15, azimuth.multiply(3, -5))
  azimuth.assert_eq(-8, azimuth.subtract(-3, 5))
  azimuth.assert_eq(8, azimuth.subtract(5, -3))
}

test "zero_handling" {
  // 测试零的处理
  azimuth.assert_eq(5, azimuth.add(5, 0))
  azimuth.assert_eq(-5, azimuth.add(-5, 0))
  azimuth.assert_eq(0, azimuth.multiply(5, 0))
  azimuth.assert_eq(0, azimuth.multiply(0, 0))
  azimuth.assert_eq(5, azimuth.subtract(5, 0))
  azimuth.assert_eq(-5, azimuth.subtract(0, 5))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 5))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(5, 0))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 商业计算：总价 = 单价 * 数量 + 税费 - 折扣
  let price = 50
  let quantity = 3
  let tax = 20
  let discount = 30
  
  let subtotal = azimuth.multiply(price, quantity)
  let total = azimuth.add(azimuth.subtract(azimuth.add(subtotal, tax), discount), 0)
  
  azimuth.assert_eq(150, subtotal)
  azimuth.assert_eq(140, total)
}

test "performance_and_large_numbers" {
  // 测试性能和大数处理
  let large_num = 1000000
  let another_large = 2000000
  
  azimuth.assert_eq(3000000, azimuth.add(large_num, another_large))
  azimuth.assert_eq(-1000000, azimuth.subtract(large_num, another_large))
  azimuth.assert_eq(2000000000000, azimuth.multiply(large_num, another_large))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(large_num, another_large))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(another_large, large_num))
}