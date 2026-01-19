// 核心 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

// 导入被测试的模块
use "azimuth"

// 测试 add 函数的基本功能
test "test_add_basic_functionality" {
  assert_eq!(5, azimuth::add(2, 3))
  assert_eq!(0, azimuth::add(0, 0))
  assert_eq!(-5, azimuth::add(-2, -3))
  assert_eq!(100, azimuth::add(50, 50))
}

// 测试 add 函数的边界情况
test "test_add_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出处理
  assert_eq!(max_val, azimuth::add(max_val, 1))
  assert_eq!(min_val, azimuth::add(min_val, -1))
  assert_eq!(-1, azimuth::add(max_val, min_val))
}

// 测试 multiply 函数的基本功能
test "test_multiply_basic_functionality" {
  assert_eq!(6, azimuth::multiply(2, 3))
  assert_eq!(0, azimuth::multiply(5, 0))
  assert_eq!(-6, azimuth::multiply(2, -3))
  assert_eq!(6, azimuth::multiply(-2, -3))
  assert_eq!(100, azimuth::multiply(10, 10))
}

// 测试 multiply 函数的边界情况
test "test_multiply_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出处理
  assert_eq!(max_val, azimuth::multiply(46341, 46341))
  assert_eq!(min_val, azimuth::multiply(-46341, 46341))
  assert_eq!(min_val, azimuth::multiply(min_val, -1))
}

// 测试 greet 函数
test "test_greet_function" {
  assert_eq!("Hello, World!", azimuth::greet("World"))
  assert_eq!("Hello, !", azimuth::greet(""))
  assert_eq!("Hello, 测试!", azimuth::greet("测试"))
  assert_eq!("Hello, MoonBit!", azimuth::greet("MoonBit"))
}

// 测试复杂数学运算
test "test_complex_mathematical_operations" {
  // 测试 (a + b) * c 的运算
  let a = 5
  let b = 3
  let c = 4
  let sum = azimuth::add(a, b)
  let result = azimuth::multiply(sum, c)
  assert_eq!(32, result)
  
  // 测试 a * b + c 的运算
  let product = azimuth::multiply(a, b)
  let result2 = azimuth::add(product, c)
  assert_eq!(19, result2)
}

// 测试实际应用场景
test "test_real_world_scenarios" {
  // 测试购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let subtotal = azimuth::multiply(item_price, quantity)
  let total = azimuth::add(subtotal, shipping)
  assert_eq!(612, total)
  
  // 测试温度转换（简化版）
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq!(80, fahrenheit)
}

// 测试字符串处理
test "test_string_processing" {
  // 测试包含特殊字符的字符串
  let special_name = "John@Doe#123"
  let result = azimuth::greet(special_name)
  assert_eq!("Hello, John@Doe#123!", result)
  
  // 测试包含空格的字符串
  let spaced_name = "Mary Jane"
  let result2 = azimuth::greet(spaced_name)
  assert_eq!("Hello, Mary Jane!", result2)
}

// 测试错误处理
test "test_error_handling" {
  // 测试断言函数
  assert_true!(true)
  assert_false!(false)
  
  // 测试数值比较
  assert_eq!(42, 42)
  assert_eq!("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  assert_true!(azimuth::add(max_val, 100) == max_val)
  assert_true!(azimuth::multiply(max_val, 2) == max_val)
}