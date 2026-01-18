// Azimuth 项目综合测试套件
// 使用标准 MoonBit 测试语法，包含10个全面的测试用例

// 定义必要的函数
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

test "add_function_with_positive_integers" {
  // 测试正整数加法
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "add_function_with_negative_integers" {
  // 测试负整数加法
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-2, -3))
  assert_eq(-100, add(-50, -50))
}

test "add_function_with_mixed_integers" {
  // 测试正负整数混合加法
  assert_eq(0, add(5, -5))
  assert_eq(-2, add(3, -5))
  assert_eq(5, add(-3, 8))
}

test "multiply_function_with_various_values" {
  // 测试各种值的乘法运算
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 10))
  assert_eq(1, multiply(1, 1))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "greet_function_with_different_names" {
  // 测试不同名称的问候功能
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, World!", greet("World"))
}

test "greet_function_with_edge_cases" {
  // 测试问候功能的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, a!", greet("a"))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式组合
  let a = 5
  let b = 3
  let c = 2
  
  // 计算 (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(16, result1)
  
  // 计算 a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(17, result2)
}

test "boundary_value_testing" {
  // 测试边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试边界值加法
  assert_eq(max_int, add(max_int, 0))
  assert_eq(min_int, add(min_int, 0))
  
  // 测试边界值乘法
  assert_eq(0, multiply(max_int, 0))
  assert_eq(0, multiply(min_int, 0))
}

test "business_calculation_scenario" {
  // 测试商业计算场景
  let unit_price = 100
  let quantity = 5
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  assert_eq(500, subtotal)
  
  // 计算最终价格（应用折扣）
  let final_price = add(subtotal, -discount)
  assert_eq(480, final_price)
}

test "geometry_calculation_scenario" {
  // 测试几何计算场景
  let length = 8
  let width = 4
  
  // 计算面积
  let area = multiply(length, width)
  assert_eq(32, area)
  
  // 计算周长
  let perimeter = multiply(add(length, width), 2)
  assert_eq(24, perimeter)
}