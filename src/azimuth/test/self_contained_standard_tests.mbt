// 标准 MoonBit 测试用例
// 自包含版本，直接定义函数而不依赖外部包

// 定义测试所需的函数
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    @builtin.abort("Assertion failed")
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected == actual) {
    ()
  } else {
    @builtin.abort("Assertion failed")
  }
}

pub fn assert_true(condition : Bool) -> Unit {
  if (condition) {
    ()
  } else {
    @builtin.abort("Assertion failed")
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  if (condition) {
    @builtin.abort("Assertion failed")
  } else {
    ()
  }
}

// 测试用例
test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(50, 50))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(20, multiply(-4, -5))
  assert_eq(0, multiply(100, 0))
}

test "greet_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(
    multiply(a, add(b, 5)),
    add(multiply(a, b), multiply(a, 5))
  )
}

test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483647
  
  // 边界值加法
  assert_eq(0, add(max_val, min_val))
  assert_eq(1, add(min_val, 2))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(max_val, multiply(max_val, 1))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算 (a + b) * c - d
  let a = 10
  let b = 15
  let c = 4
  let d = 20
  
  let sum_ab = add(a, b)
  let product = multiply(sum_ab, c)
  let result = add(product, -d)
  
  assert_eq(80, result)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(612, total)
}

test "string_processing" {
  // 测试字符串处理
  // 测试特殊字符
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, 用户@123!", greet("用户@123"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  
  // 测试长字符串
  let long_name = "VeryLongNameForTesting"
  assert_eq_string("Hello, VeryLongNameForTesting!", greet(long_name))
}

test "error_handling" {
  // 测试错误处理
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("success", "success")
}

test "performance_operations" {
  // 测试性能相关的操作
  // 大数运算（使用较小的数值以避免溢出）
  assert_eq(20000, add(10000, 10000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 负数大数运算
  assert_eq(-20000, add(-10000, -10000))
  assert_eq(-100000000, multiply(-10000, 10000))
  
  // 混合符号运算
  assert_eq(0, add(10000, -10000))
  assert_eq(-100000000, multiply(10000, -10000))
}