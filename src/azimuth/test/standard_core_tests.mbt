// 标准 MoonBit 核心测试用例
// 测试 Azimuth 项目的核心功能

test "add_function_basic" {
  // 测试 add 函数的基本功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_basic" {
  // 测试 multiply 函数的基本功能
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
}

test "greet_function_variations" {
  // 测试 greet 函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 2)), 
                    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 2)))
}

test "boundary_values" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算：(a + b) * c - d
  let a = 10
  let b = 15
  let c = 4
  let d = 30
  
  let sum_ab = @azimuth.add(a, b)  // 25
  let product = @azimuth.multiply(sum_ab, c)  // 100
  let result = @azimuth.add(product, -d)  // 70
  
  @azimuth.assert_eq(70, result)
}

test "sequential_operations" {
  // 测试连续操作
  // 连续加法：1 + 2 + 3 + 4
  let sum_result = @azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4)
  @azimuth.assert_eq(10, sum_result)
  
  // 连续乘法：2 * 3 * 4
  let product_result = @azimuth.multiply(@azimuth.multiply(2, 3), 4)
  @azimuth.assert_eq(24, product_result)
}

test "string_processing" {
  // 测试字符串处理
  // 包含特殊字符的字符串
  let special_chars = "@#$%"
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet(special_chars))
  
  // 包含空格的字符串
  let spaced_name = "Alice Bob"
  @azimuth.assert_eq_string("Hello, Alice Bob!", @azimuth.greet(spaced_name))
  
  // 包含数字和字母的字符串
  let alphanumeric = "User123"
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet(alphanumeric))
}

test "real_world_scenario" {
  // 测试实际应用场景：计算订单总价
  let item_price = 199
  let quantity = 2
  let shipping = 20
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 计算最终总价
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(418, total)  // 199*2 + 20 = 398 + 20 = 418
}

test "error_handling" {
  // 测试错误处理和边界情况
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}