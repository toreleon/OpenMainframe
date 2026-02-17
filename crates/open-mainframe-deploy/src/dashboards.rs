//! Pre-built Grafana dashboard JSON and Prometheus alerting rules.
//!
//! Provides ready-to-import monitoring artifacts for observability:
//! - Grafana dashboards for COBOL, CICS, IMS, and database metrics
//! - Prometheus alert rules for latency, pool exhaustion, and health

/// Configuration for dashboard generation.
#[derive(Debug, Clone)]
pub struct DashboardConfig {
    /// Prometheus datasource name in Grafana.
    pub datasource: String,
    /// Metrics prefix (must match MetricsRegistry prefix).
    pub prefix: String,
    /// Dashboard title.
    pub title: String,
}

impl Default for DashboardConfig {
    fn default() -> Self {
        Self {
            datasource: "Prometheus".to_string(),
            prefix: "open_mainframe".to_string(),
            title: "OpenMainframe Overview".to_string(),
        }
    }
}

/// Generate a Grafana dashboard JSON model.
///
/// The returned JSON can be imported directly into Grafana via the
/// dashboard import API or the UI.
pub fn generate_grafana_dashboard(config: &DashboardConfig) -> String {
    let prefix = &config.prefix;
    let ds = &config.datasource;

    format!(
        r#"{{
  "dashboard": {{
    "title": "{title}",
    "uid": "open-mainframe-overview",
    "tags": ["open-mainframe", "cobol", "cics"],
    "timezone": "browser",
    "refresh": "10s",
    "panels": [
      {{
        "title": "Transaction Rate",
        "type": "timeseries",
        "gridPos": {{ "h": 8, "w": 12, "x": 0, "y": 0 }},
        "datasource": "{ds}",
        "targets": [
          {{
            "expr": "rate({prefix}_cics_transactions_total[5m])",
            "legendFormat": "{{{{transaction}}}}"
          }}
        ]
      }},
      {{
        "title": "Latency Percentiles",
        "type": "timeseries",
        "gridPos": {{ "h": 8, "w": 12, "x": 12, "y": 0 }},
        "datasource": "{ds}",
        "targets": [
          {{
            "expr": "histogram_quantile(0.50, rate({prefix}_cics_transaction_duration_bucket[5m]))",
            "legendFormat": "p50"
          }},
          {{
            "expr": "histogram_quantile(0.95, rate({prefix}_cics_transaction_duration_bucket[5m]))",
            "legendFormat": "p95"
          }},
          {{
            "expr": "histogram_quantile(0.99, rate({prefix}_cics_transaction_duration_bucket[5m]))",
            "legendFormat": "p99"
          }}
        ]
      }},
      {{
        "title": "Error Rate",
        "type": "timeseries",
        "gridPos": {{ "h": 8, "w": 12, "x": 0, "y": 8 }},
        "datasource": "{ds}",
        "targets": [
          {{
            "expr": "rate({prefix}_errors_total[5m])",
            "legendFormat": "{{{{type}}}}"
          }}
        ]
      }},
      {{
        "title": "Active Connections",
        "type": "stat",
        "gridPos": {{ "h": 8, "w": 6, "x": 12, "y": 8 }},
        "datasource": "{ds}",
        "targets": [
          {{
            "expr": "{prefix}_db_pool_connections",
            "legendFormat": "Pool Connections"
          }}
        ]
      }},
      {{
        "title": "Queue Depth",
        "type": "gauge",
        "gridPos": {{ "h": 8, "w": 6, "x": 18, "y": 8 }},
        "datasource": "{ds}",
        "targets": [
          {{
            "expr": "{prefix}_cics_queue_depth",
            "legendFormat": "CICS Queue"
          }}
        ]
      }},
      {{
        "title": "COBOL Programs Executed",
        "type": "timeseries",
        "gridPos": {{ "h": 8, "w": 12, "x": 0, "y": 16 }},
        "datasource": "{ds}",
        "targets": [
          {{
            "expr": "rate({prefix}_cobol_programs_executed_total[5m])",
            "legendFormat": "{{{{program}}}}"
          }}
        ]
      }},
      {{
        "title": "Batch Job Return Codes",
        "type": "table",
        "gridPos": {{ "h": 8, "w": 12, "x": 12, "y": 16 }},
        "datasource": "{ds}",
        "targets": [
          {{
            "expr": "{prefix}_batch_job_return_code",
            "legendFormat": "{{{{job_name}}}} RC",
            "instant": true
          }}
        ]
      }}
    ],
    "schemaVersion": 38,
    "version": 1
  }},
  "overwrite": true
}}"#,
        title = config.title,
    )
}

/// Alert configuration for threshold customization.
#[derive(Debug, Clone)]
pub struct AlertConfig {
    /// Metrics prefix.
    pub prefix: String,
    /// P99 latency threshold in seconds.
    pub latency_threshold_s: f64,
    /// Duration for latency alert to fire.
    pub latency_for: String,
    /// Connection pool utilization percentage to alert.
    pub pool_utilization_pct: u32,
    /// Duration for readiness failure alert.
    pub readiness_for: String,
}

impl Default for AlertConfig {
    fn default() -> Self {
        Self {
            prefix: "open_mainframe".to_string(),
            latency_threshold_s: 1.0,
            latency_for: "5m".to_string(),
            pool_utilization_pct: 90,
            readiness_for: "1m".to_string(),
        }
    }
}

/// Generate Prometheus alerting rules in YAML format.
///
/// The returned YAML can be loaded by Prometheus as a rules file.
pub fn generate_alert_rules(config: &AlertConfig) -> String {
    let prefix = &config.prefix;
    format!(
        r#"groups:
  - name: open_mainframe_alerts
    rules:
      - alert: HighP99Latency
        expr: histogram_quantile(0.99, rate({prefix}_cics_transaction_duration_bucket[5m])) > {latency_threshold}
        for: {latency_for}
        labels:
          severity: warning
        annotations:
          summary: "High p99 transaction latency"
          description: "CICS transaction p99 latency has exceeded {latency_threshold}s for {latency_for}."

      - alert: ConnectionPoolExhaustion
        expr: ({prefix}_db_pool_connections / {prefix}_db_pool_max_connections) * 100 > {pool_pct}
        for: 2m
        labels:
          severity: critical
        annotations:
          summary: "Database connection pool near exhaustion"
          description: "Connection pool utilization is above {pool_pct}%."

      - alert: ReadinessCheckFailing
        expr: up{{job="open-mainframe"}} == 0 or {prefix}_health_ready == 0
        for: {readiness_for}
        labels:
          severity: critical
        annotations:
          summary: "OpenMainframe readiness check failing"
          description: "The readiness endpoint has been failing for {readiness_for}."

      - alert: HighBatchJobFailureRate
        expr: rate({prefix}_batch_jobs_failed_total[15m]) / rate({prefix}_batch_jobs_completed_total[15m]) > 0.2
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "High batch job failure rate"
          description: "More than 20% of batch jobs are failing over the last 15 minutes."

      - alert: CicsQueueBacklog
        expr: {prefix}_cics_queue_depth > 100
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "CICS queue depth high"
          description: "CICS queue depth has been above 100 for 5 minutes."
"#,
        latency_threshold = config.latency_threshold_s,
        latency_for = config.latency_for,
        pool_pct = config.pool_utilization_pct,
        readiness_for = config.readiness_for,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_dashboard_default() {
        let config = DashboardConfig::default();
        let json = generate_grafana_dashboard(&config);
        assert!(json.contains("\"title\": \"OpenMainframe Overview\""));
        assert!(json.contains("Transaction Rate"));
        assert!(json.contains("Latency Percentiles"));
        assert!(json.contains("Error Rate"));
        assert!(json.contains("Active Connections"));
        assert!(json.contains("Queue Depth"));
    }

    #[test]
    fn test_dashboard_uses_custom_prefix() {
        let config = DashboardConfig {
            prefix: "myapp".to_string(),
            ..Default::default()
        };
        let json = generate_grafana_dashboard(&config);
        assert!(json.contains("myapp_cics_transactions_total"));
        assert!(json.contains("myapp_errors_total"));
    }

    #[test]
    fn test_dashboard_uses_custom_datasource() {
        let config = DashboardConfig {
            datasource: "MyProm".to_string(),
            ..Default::default()
        };
        let json = generate_grafana_dashboard(&config);
        assert!(json.contains("\"datasource\": \"MyProm\""));
    }

    #[test]
    fn test_dashboard_is_valid_json() {
        let config = DashboardConfig::default();
        let json = generate_grafana_dashboard(&config);
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert!(parsed["dashboard"]["panels"].is_array());
        assert_eq!(parsed["dashboard"]["panels"].as_array().unwrap().len(), 7);
    }

    #[test]
    fn test_dashboard_panel_types() {
        let config = DashboardConfig::default();
        let json = generate_grafana_dashboard(&config);
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        let panels = parsed["dashboard"]["panels"].as_array().unwrap();
        let types: Vec<&str> = panels
            .iter()
            .map(|p| p["type"].as_str().unwrap())
            .collect();
        assert!(types.contains(&"timeseries"));
        assert!(types.contains(&"stat"));
        assert!(types.contains(&"gauge"));
        assert!(types.contains(&"table"));
    }

    #[test]
    fn test_generate_alert_rules_default() {
        let config = AlertConfig::default();
        let yaml = generate_alert_rules(&config);
        assert!(yaml.contains("HighP99Latency"));
        assert!(yaml.contains("ConnectionPoolExhaustion"));
        assert!(yaml.contains("ReadinessCheckFailing"));
        assert!(yaml.contains("HighBatchJobFailureRate"));
        assert!(yaml.contains("CicsQueueBacklog"));
    }

    #[test]
    fn test_alert_rules_custom_thresholds() {
        let config = AlertConfig {
            latency_threshold_s: 2.5,
            pool_utilization_pct: 80,
            readiness_for: "2m".to_string(),
            ..Default::default()
        };
        let yaml = generate_alert_rules(&config);
        assert!(yaml.contains("> 2.5"));
        assert!(yaml.contains("> 80"));
        assert!(yaml.contains("for: 2m"));
    }

    #[test]
    fn test_alert_rules_uses_prefix() {
        let config = AlertConfig {
            prefix: "custom".to_string(),
            ..Default::default()
        };
        let yaml = generate_alert_rules(&config);
        assert!(yaml.contains("custom_cics_transaction_duration_bucket"));
        assert!(yaml.contains("custom_db_pool_connections"));
    }

    #[test]
    fn test_alert_rules_severity_levels() {
        let config = AlertConfig::default();
        let yaml = generate_alert_rules(&config);
        assert!(yaml.contains("severity: warning"));
        assert!(yaml.contains("severity: critical"));
    }
}
