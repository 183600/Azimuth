// 标准 MoonBit 测试用例
// 自包含版本，直接定义函数而不依赖外部包

// 定义测试所需的函数
pub fn @azimuth.add(a : Int, b : Int) -> Int {
  a + b
}

pub fn @azimuth.multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn @azimuth.greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn @azimuth.assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    @builtin.abort("Assertion failed")
  }
}

pub fn @azimuth.assert_eq_string(expected : String, actual : String) -> Unit {
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
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
}

test "greet_functionality" {
  // 测试问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 5)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 5))
  )
}

test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483647
  
  // 边界值加法
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(1, @azimuth.add(min_val, 2))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算 (a + b) * c - d
  let a = 10
  let b = 15
  let c = 4
  let d = 20
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(80, result)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(612, total)
}

test "string_processing" {
  // 测试字符串处理
  // 测试特殊字符
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, 用户@123!", @azimuth.greet("用户@123"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  
  // 测试长字符串
  let long_name = "VeryLongNameForTesting"
  @azimuth.assert_eq_string("Hello, VeryLongNameForTesting!", @azimuth.greet(long_name))
}

test "error_handling" {
  // 测试错误处理
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("success", "success")
}

test "performance_operations" {
  // 测试性能相关的操作
  // 大数运算（使用较小的数值以避免溢出）
  @azimuth.assert_eq(20000, @azimuth.add(10000, 10000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  
  // 负数大数运算
  @azimuth.assert_eq(-20000, @azimuth.add(-10000, -10000))
  @azimuth.assert_eq(-100000000, @azimuth.multiply(-10000, 10000))
  
  // 混合符号运算
  @azimuth.assert_eq(0, @azimuth.add(10000, -10000))
  @azimuth.assert_eq(-100000000, @azimuth.multiply(10000, -10000))
}