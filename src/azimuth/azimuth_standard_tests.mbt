// Azimuth 项目标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

// 导入 azimuth 模块的函数
use azimuth::{add, multiply, subtract, divide_with_ceil, greet, assert_eq, assert_eq_string}

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "divide_with_ceil_positive_numbers" {
  // 向上取整除法正数测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(a, b))
  assert_eq(96, multiply(a, b))
}

test "edge_cases_and_boundary_values" {
  // 边界值和特殊情况测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 单位值测试
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, divide_with_ceil(5, 1))
  
  // 负数边界测试
  assert_eq(-5, add(0, -5))
  assert_eq(-5, multiply(-5, 1))
  assert_eq(-5, subtract(0, 5))
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  let x = 10
  let y = 3
  let z = 2
  
  // 复合运算：(x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(26, result1)
  
  // 复合运算：x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(32, result2)
  
  // 复合运算：(x - y) / z (向上取整)
  let result3 = divide_with_ceil(subtract(x, y), z)
  assert_eq(4, result3)
  
  // 嵌套运算：x + (y * (z + 1))
  let result4 = add(x, multiply(y, add(z, 1)))
  assert_eq(19, result4)
}

test "real_world_application_simulation" {
  // 真实世界应用模拟测试
  // 购物车计算场景
  let item_price = 25
  let quantity = 4
  let tax_rate = 10
  
  // 计算小计
  let subtotal = multiply(item_price, quantity)
  assert_eq(100, subtotal)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  assert_eq(10, tax)
  
  // 计算总价
  let total = add(subtotal, tax)
  assert_eq(110, total)
  
  // 计算每箱包装数量（假设每箱最多6个）
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(quantity, items_per_box)
  assert_eq(1, boxes_needed)
  
  // 验证问候功能在客户服务场景中的应用
  let customer_name = "张三"
  let greeting_message = greet(customer_name)
  assert_eq_string("Hello, 张三!", greeting_message)
}