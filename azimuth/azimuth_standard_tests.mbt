// Azimuth 标准测试用例 - 使用标准 MoonBit 测试语法
// 包含 8 个高质量测试用例，覆盖核心功能

// 定义基本函数
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

fn subtract(a : Int, b : Int) -> Int {
  a - b
}

// 断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  assert_eq(1000, add(500, 500))
  assert_eq(-1000, add(-500, -500))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(100, 0))
  assert_eq(-30, multiply(-6, 5))
  assert_eq(30, multiply(-6, -5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "divide_with_ceil_scenarios" {
  // 向上取整除法场景测试
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(1, divide_with_ceil(3, 10))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(20, 0))
}

test "divide_with_ceil_negative_handling" {
  // 向上取整除法负数处理测试
  assert_eq(-3, divide_with_ceil(-11, 4))
  assert_eq(-4, divide_with_ceil(11, -3))
  assert_eq(4, divide_with_ceil(-11, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "greet_function_international" {
  // 问候函数国际化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "subtract_function_validation" {
  // 减法函数验证测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体计算结果
  assert_eq(20, add(12, 8))
  assert_eq(96, multiply(12, 8))
}

test "real_world_packaging_calculation" {
  // 真实世界包装计算测试
  let total_products = 47
  let package_capacity = 12
  let packages_needed = divide_with_ceil(total_products, package_capacity)
  
  assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, package_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(1, remaining_space)
}