下面给你一个“OpenTelemetry 风格”的 MoonBit Telemetry 套件实现方案（偏工程落地），目标是做出 **API/SDK 分离、三大信号（Trace/Metrics/Logs）、上下文传播（W3C TraceContext + Baggage）、可插拔 Exporter（优先 OTLP/HTTP）** 的完整闭环；并且充分利用 MoonBit 的 **多后端 + async/结构化并发** 能力。MoonBit 有多后端（Wasm / WasmGC / JS / C / Native / LLVM 实验）与 FFI 机制，你的设计最好从第一天就把“跨后端差异”隔离出去。 

---

## 0. 总体设计原则（对齐 OpenTelemetry 的“味道”）

1. **API / SDK 分离**
   - `telemetry/api/*`：给业务与 instrumentation 用，默认 no-op，不依赖网络/线程/IO。
   - `telemetry/sdk/*`：真正做采样、聚合、队列、批处理、导出（OTLP/HTTP 等）。
   - 这样可以做到：只引入 API 不会带来性能/依赖负担；SDK 可替换、可裁剪。

2. **Context 作为“贯穿三信号的骨架”**
   - TraceContext（traceparent/tracestate）用于分布式链路关联；Baggage 用于跨进程携带业务标签。 
   - Metrics/Logs 也应能从 Context 里取出 TraceId/SpanId（日志自动关联 trace 等）。 

3. **SDK 的后台工作都走 async + 结构化并发**
   - BatchSpanProcessor / PeriodicMetricReader / BatchLogProcessor 都需要后台 loop、定时 flush、失败重试。
   - MoonBit 已提供 async/协程/TaskGroup 结构化并发能力，适合做“后台 exporter worker”。 

4. **导出优先 OTLP/HTTP**
   - OTLP 规范明确了 `/v1/traces`、`/v1/metrics`、`/v1/logs` 路径与 Protobuf/JSON 编码等。 
   - MoonBit 生态已有 `moonbitlang/protobuf` 的读写能力，可用来写 OTLP Protobuf payload。 
   - MoonBit async 库的 HTTP 客户端在 2025-12 的更新里提到支持代理（CONNECT/鉴权等），对企业网络环境很关键。 

---

## 1. 包/模块拆分建议（MoonBit 工程结构）

MoonBit 有 module/package 体系 + `moon` 包管理，建议做一个顶层 module，例如 `yourname/telemetry`，内部按包拆分（路径示意）： 

### 1.1 API 层（稳定、轻量、默认 no-op）
- `yourname/telemetry/api/common`
  - `Attributes`, `AttributeValue`, `Resource`, `InstrumentationScope`
- `yourname/telemetry/api/context`
  - `Context`, `ContextKey[T]`, `Baggage`
- `yourname/telemetry/api/propagation`
  - `TextMapCarrier`, `TextMapPropagator`, `CompositePropagator`
  - `W3CTraceContextPropagator`, `W3CBaggagePropagator`
- `yourname/telemetry/api/trace`
  - `TracerProvider`, `Tracer`, `Span`, `SpanContext`, `SpanKind`, `Status`, `Event`
- `yourname/telemetry/api/metrics`
  - `MeterProvider`, `Meter`, instruments（Counter/Histogram/UpDownCounter/Gauge…）
- `yourname/telemetry/api/logs`
  - `LoggerProvider`, `Logger`, `LogRecord`

### 1.2 SDK 层（可选依赖、可替换）
- `yourname/telemetry/sdk/common`
  - limits、clock、runtime 抽象、shared queue、batching util
- `yourname/telemetry/sdk/trace`
  - `SdkTracerProvider`, `Sampler`, `SpanProcessor`（Simple/Batch），`IdGenerator`
- `yourname/telemetry/sdk/metrics`
  - `SdkMeterProvider`, `View`, `Aggregation`, `MetricReader`（Periodic），temporality
- `yourname/telemetry/sdk/logs`
  - `SdkLoggerProvider`, `LogRecordProcessor`（Simple/Batch）
- `yourname/telemetry/sdk/exporter/*`
  - `console`
  - `otlp_http`（Protobuf 编码 + HTTP POST）
  - （后续）`otlp_grpc`、`file`、`prometheus`（如果要）

### 1.3 与后端相关的 platform 包（强隔离）
- `yourname/telemetry/sdk/platform/time`：获取 epoch nanos
- `yourname/telemetry/sdk/platform/random`：随机数/安全随机（trace/span id）
- `yourname/telemetry/sdk/platform/http`：对接 `moonbitlang/async/http` 或 JS fetch 等

> 关键点：**API 不碰 FFI/IO**；SDK 通过 platform 包屏蔽不同 backend 差异。MoonBit 的 FFI/后端差异是真实存在的（例如 wasm 后端不支持 `extern "C"` 这种用法在一些场景会直接报错），所以隔离很重要。 

---

## 2. 核心数据结构设计（Attributes / Resource / Scope）

### 2.1 Attributes
- 设计一个 `AttributeValue` sum type（enum）：
  - `Int64`, `Float64`, `Bool`, `String`
  - `ArrayInt64`, `ArrayFloat64`, `ArrayBool`, `ArrayString`
- `Attributes`：
  - API 层用不可变 `Array[(String, AttributeValue)]`（小对象、易分享）
  - SDK 内部可用 `Map[String, AttributeValue]` 做合并、去重、limit 裁剪

### 2.2 Resource（服务级别元信息）
- 默认资源至少自动填：
  - `service.name`（若用户没填，按规范 fallback）
  - `telemetry.sdk.*`（language/name/version） 
- Resource 是 **进程/实例级别**，挂在 Provider 上（TracerProvider/MeterProvider/LoggerProvider），由 SDK 在导出时附上。Trace/Logs SDK 都明确提到 Provider 应能绑定 Resource。 

### 2.3 InstrumentationScope
- 创建 `Tracer`/`Logger` 时保存 scope（name/version/schema_url），trace/log SDK 都强调要从 Provider 创建并存 scope。 

---

## 3. Context 与传播（W3C TraceContext + Baggage）

### 3.1 Context API（强烈建议走“显式传递”）
MoonBit 的 async 是协程 + 任务切换，做“隐式 TLS/协程本地变量”的成本和坑都很高；建议学习 Go OTel 风格：**所有关键入口都允许显式传 Context**，并提供少量语法糖。

- `Context`：不可变对象（内部结构可用持久化 map 或链表栈）
- `ContextKey[T]`：typed key
- `Context.with_value(key, value)` / `Context.get(key)`

### 3.2 SpanContext
- 字段：`trace_id(16 bytes)`, `span_id(8 bytes)`, `trace_flags(8-bit)`, `tracestate(String)`
- 传播遵循 W3C `traceparent` 的 version/trace-id/parent-id/flags 格式校验。 

### 3.3 Propagators
实现：
- `W3CTraceContextPropagator`：注入/提取 `traceparent`、`tracestate`，严格按要求验证与忽略非法值。 
- `W3CBaggagePropagator`：注入/提取 `baggage` header，Baggage API 强调 name/value 唯一、冲突覆盖等。 
- `CompositePropagator`：默认把 TraceContext + Baggage 组合（OTel 推荐默认组合）。 

Carrier 设计：
- `trait TextMapCarrier { get(key): String?; set(key, value): Unit; keys(): Array[String] }`
- 提供 `HttpHeadersCarrier(Map[String,String])` 的适配器。

---

## 4. Tracing SDK（Span / Sampler / Processor / Exporter）

### 4.1 对齐 OTel Trace SDK 的关键语义
- `IsRecording` 与 `Sampled` 的组合必须符合规范（不允许 `Sampled=true` 但 `IsRecording=false`）。 
- `Sampler`：实现 `ShouldSample` 返回 `DROP / RECORD_ONLY / RECORD_AND_SAMPLE`。 
- `SpanProcessor`：
  - `SimpleSpanProcessor`：结束即导出（同步串行调用 exporter）
  - `BatchSpanProcessor`：队列 + 定时 + 批量（并保证同一个 exporter 的 Export 不并发）。 

### 4.2 SdkTracerProvider
配置项（建议 builder）：
- `resource: Resource`
- `sampler: Sampler`（默认 ParentBased(AlwaysOn) 的行为可后补）
- `id_generator`
- `span_limits`（attribute count、event count、link count…）
- `processors: Array[SpanProcessor]`

创建 tracer：
- `get_tracer(name, version?, schema_url?) -> Tracer`（保存 scope） 

### 4.3 Span 实现策略（可读/可写分离）
- `Span`（API 暴露）：
  - `set_attribute`
  - `add_event`
  - `record_exception`
  - `set_status`
  - `end(end_time?)`
- `ReadableSpan`（SDK 内部给 processor/exporter）
  - 结束后 immutable snapshot（避免并发/异步读写问题）

### 4.4 BatchSpanProcessor 的 async 实现（MoonBit 很适合）
用 `moonbitlang/async` 做后台 worker：
- 一个 `Queue[ReadableSpan]`
- 一个 `spawn_loop`：每 `scheduledDelayMillis` 触发 flush，或队列满触发 flush
- flush 时串行调用 exporter.Export（规范要求同 exporter 不并发） 

---

## 5. Metrics SDK（Meter / View / Aggregation / Reader）

Metrics 复杂度很高，建议分阶段实现，但架构先搭对。

### 5.1 对齐 OTel Metrics SDK 核心概念
- Instrument 属于某个 `MeterProvider`
- View 决定 stream 配置（name/attr keys/aggregation/temporality）
- 需要处理基数限制与 overflow 属性点（`otel.metric.overflow=true`）等规则 

### 5.2 最小可用版本（MVP）建议
先做：
- Instruments：Counter、Histogram、UpDownCounter（同步）
- Aggregations：Sum、ExplicitBucketHistogram（简化版）
- Reader：PeriodicExportingMetricReader（定时收集导出）
- Exporter：Console、OTLP/HTTP

后续再加：
- Async instruments（ObservableGauge 等）
- Exemplars（TraceBased/AlwaysOn/AlwaysOff） 
- 更完整 View 语法与冲突处理

### 5.3 实现骨架
- `SdkMeterProvider`
  - holds: resource, views, readers
- `Instrument` 记录 measurement → 路由到对应 aggregator（按 view 生成 stream）
- `collect()`：
  - 从 aggregator snapshot 出 `MetricData`
  - reader/exporter 输出

---

## 6. Logs SDK（Logger / Processor / Exporter）

Logs 建议和 Trace 类似做“Processor 管线 + Exporter”。

### 6.1 对齐 Logs SDK 要点
- `LoggerProvider` 拥有配置（processors 等），Logger 只是产生 LogRecord。 
- `LogRecordProcessor`：Simple/Batch 两种都要有，且同 exporter 的 Export 不并发。 
- LogRecord 需要能从 Context 填 trace 关联字段（TraceId/SpanId/TraceFlags）。 

---

## 7. OTLP/HTTP Exporter（重点：可落地、跨后端）

### 7.1 OTLP/HTTP 协议要点
- HTTP POST
- traces → `/v1/traces`，metrics → `/v1/metrics`，logs → `/v1/logs`
- body 支持 Protobuf binary 或 JSON（Protobuf-JSON）
- 可选 gzip（Content-Encoding: gzip） 

### 7.2 Protobuf 编码路线
推荐走 **Protobuf binary**（体积小、性能好）：
1. 用 OTLP 的 `.proto` 生成 MoonBit 数据类型（可参考社区里“用 MoonBit + protobuf 读写消息”的做法，里面直接 `@protobuf.Write::write(msg, writer)` 输出二进制）。   
2. exporter 里构造：
   - `ExportTraceServiceRequest { resource_spans: [...] }`
   - `ExportMetricsServiceRequest { resource_metrics: [...] }`
   - `ExportLogsServiceRequest { resource_logs: [...] }`
3. `@protobuf.Write::write(req, buffer)` → `Bytes`
4. HTTP client 发出

> 你不需要一开始就把 OTLP proto 全量实现（字段很多）。可以先实现“最常用子集”：Resource、Scope、Span、Metric（Sum/Histogram）、LogRecord。

### 7.3 HTTP client 选型（MoonBit）
- Native：优先用 `moonbitlang/async/http`（并可利用其代理能力），做 async exporter worker。 
- JS：MoonBit async 在 2025-12 的月报里提到 **JS 后端实验性支持**，但 IO 覆盖范围要谨慎评估（可以先在 JS 上只提供 ConsoleExporter / InMemoryExporter）。 
- Wasm：通常依赖宿主导入函数（WASI/JS glue）。MoonBit FFI 文档也强调 wasm 与外界交互靠宿主。 

---

## 8. “跨后端时间/随机数”抽象（务必第一天就做）

OTLP 里 span/log 都需要 epoch 时间戳（纳秒），trace/span id 需要随机/伪随机。

建议做：
- `trait Clock { now_unix_nanos() -> Int64 }`
  - `#cfg(target="native")`：用 C FFI 调 `clock_gettime` 或 `gettimeofday`（或用现成库封装）
  - `#cfg(target="js")`：JS FFI 调 `Date.now()` 再转 nanos
  - `#cfg(target="wasm")`：从宿主 import（WASI 或 JS glue）
- `trait Random { next_u64(); fill_bytes(buf) }`
  - native 可用系统随机或快速 PRNG（注意 trace id 的碰撞风险）
  - wasm/js 走宿主 `crypto.getRandomValues` 等

隔离原因：在 wasm 上直接 `extern "C"` 之类能力可能不可用（社区文章里就出现过 wasm 后端对某些 `extern "C"` 方式不支持的报错），所以必须把 platform 代码集中管理。 

---

## 9. 配置与默认值（做成 Builder，像 OTel SDK）

建议提供三套入口：

1. **纯 API（默认 no-op）**
   - 不初始化 SDK 时，所有 `global_tracer()` 返回 NoopTracer（零开销或接近零开销）。

2. **SDK Builder（代码配置）**
   - `Sdk::builder().with_resource(...).with_otlp_http_exporter(...).build_and_set_global()`

3. **Env 配置（后续增强）**
   - 逐步支持 `OTEL_EXPORTER_OTLP_ENDPOINT` 等（可以先不做全量）

---

## 10. 关键接口草图（MoonBit 伪代码风格）

> 下面是“形状”，不是可直接编译的最终代码；重点是接口边界与依赖方向。

```mbt
// api/trace
pub trait Span {
  fn span_context(self) -> SpanContext
  fn is_recording(self) -> Bool
  fn set_attribute(self, key: String, value: @common.AttributeValue) -> Unit
  fn add_event(self, name: String, attrs?: @common.Attributes, time_unix_nanos?: Int64) -> Unit
  fn set_status(self, code: StatusCode, description?: String) -> Unit
  fn end(self, end_time_unix_nanos?: Int64) -> Unit
}

pub trait Tracer {
  fn start_span(
    self,
    ctx: @context.Context,
    name: String,
    kind?: SpanKind,
    attrs?: @common.Attributes,
    start_time_unix_nanos?: Int64,
  ) -> (@context.Context, Span)
}

pub trait TracerProvider {
  fn get_tracer(self, name: String, version?: String) -> Tracer
}
```

```mbt
// sdk/trace
pub trait SpanExporter {
  async fn export(self, spans: Array[ReadableSpan]) -> ExportResult
  async fn force_flush(self) -> ExportResult
  async fn shutdown(self) -> ExportResult
}

pub trait SpanProcessor {
  fn on_start(self, span: ReadWriteSpan, parent: @context.Context) -> Unit
  fn on_end(self, span: ReadableSpan) -> Unit
  async fn force_flush(self, timeout_ms: Int) -> Bool
  async fn shutdown(self, timeout_ms: Int) -> Bool
}
```

```mbt
// sdk/exporter/otlp_http
pub struct OtlpHttpExporter {
  endpoint: String
  headers: Map[String, String]
  // http client + retry policy + compression config ...
}

pub async fn OtlpHttpExporter::export_traces(self, req: ExportTraceServiceRequest) -> ExportResult {
  let body = @protobuf.encode(req) // 你内部封装 @protobuf.Write::write(...) 
  // POST self.endpoint + "/v1/traces"  
  // use @http.post(...) from moonbitlang/async/http  
}
```

---

## 11. 推荐的落地路线图（避免一口吃成胖子）

### Phase A（1–2 周）：Trace MVP
- API：Context + Trace API（Tracer/Span）
- SDK：SdkTracerProvider + AlwaysOnSampler + SimpleSpanProcessor
- Exporter：ConsoleExporter（打印 span）
- Propagation：W3C traceparent parse/inject（先不做 tracestate 复杂合并）

> 这一步结束你就能跑通“本地链路追踪 + header 传播”。

### Phase B（2–4 周）：OTLP/HTTP Trace
- 引入 `moonbitlang/protobuf` 写 OTLP Trace Protobuf
- 实现 OTLP/HTTP exporter `/v1/traces` 
- BatchSpanProcessor（队列 + 定时 flush），用 `moonbitlang/async` 做后台任务 

### Phase C（4–8 周）：Metrics MVP + OTLP/HTTP Metrics
- Counter/Histogram + PeriodicReader
- OTLP metrics exporter `/v1/metrics` 

### Phase D（8–12 周）：Logs MVP + OTLP/HTTP Logs
- LoggerProvider/Logger + BatchLogProcessor
- LogRecord 自动附带 trace 关联字段（从 Context 取） 
- OTLP logs exporter `/v1/logs` 

---

## 12. 最容易踩坑的点（提前规避）

1. **“当前 Context”不要强依赖隐式全局**
   - async 并发下，全局变量会串；建议明确要求业务把 `Context` 往下传（或你提供 `with_context(ctx, f)` + 你自己 spawn wrapper）。

2. **时间戳与单调时钟**
   - OTel/OTLP 需要 Unix epoch；但很多运行时提供的是单调时间（只适合算 duration）。要明确区分，并用 `Clock` 抽象统一。 

3. **Exporter 并发与批处理**
   - 规范强调同一个 exporter 的 Export 不应并发调用（Trace/Logs 都强调）。Batch processor 一定要串行化 exporter 调用。 

4. **语义约定（service.name 等）**
   - Resource 里 `service.name`、`telemetry.sdk.*` 属于“基础盘”，很多后端展示/聚合依赖它。 

---

如果你按这个方案推进，基本能做出一个“看起来、用起来、可扩展性”都很 OpenTelemetry 的 MoonBit Telemetry SDK：**业务依赖 API，运行时替换 SDK；Context/Propagator 串起 Trace/Metrics/Logs；OTLP/HTTP 直连 Collector；后台批处理靠 async/TaskGroup 稳定运行**。 