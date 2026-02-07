#!/usr/bin/env bash
# Run all doctest modules. Uses run_eunit.sh for reliable compile.
set -e
cd "$(dirname "$0")/.."

# All doctest modules
MODULES="cre wf_engine pnet_choice wf_timer cre_yawl_client cre_config wf_conc wf_store wf_worklet wf_timerq yawl_timeout yawl_reporter yawl_simulation cre_yawl_patterns wf_choice wf_data wf_scope pnet_types pnet_receipt yawl_cost yawl_twitter yawl_ipc wf_audit_log wf_calendar gen_pnet gen_yawl yawl_compile yawl_compiled yawl_engine yawl_executor yawl_xes wf_persistence wf_rules wf_task yawl_execution yawl_mi_runtime wf_test_net_basic wf_test_net_choice wf_test_stub_net wf_test_net_task_gate wf_yawl_pred yawl_pred_eval yawl_state pnet_marking pnet_mode deferred_choice exclusive_choice simple_merge wf_ipc wf_file wf_forms wf_spec yawl_worklet yawl_wsif yawl_stateless yawl_scheduling yawl_resourcing yawl_monitor yawl_sms yawl_otel_logger yawl_telemetry yawl_web_dashboard cre_yawl_worker cre_yawl_patterns discrimininator implicit_merge milestone multiple_merge or_join parallel_split interleaved_routing n_out_of_m yawl_approval yawl_auth yawl_control yawl_balancer yawl_documents yawl_elements yawl_marshal yawl_schema yawl_util wf_verify wf_xes wf_time wf_services wf_resource wf_cancel wf_exception wf_pool wf_ops wf_mi wf_prop wf_multi_instance wf_try_region yawl_approval_middleware yawl_signature yawl_logging yawl_mail yawl_pattern_reference yawl_persistence cre_client cre_master cre_worker cre_yawl cre_yawl_exception cre_yawl_persistence wf_test_net_receipt wf_test_net_resume wf_test_net_trigger_drop cre_sup telemetry yawl_claude_bridge cre_history_handler cre_status_handler wf_admin_api gen_smtp_client yawl_interface_d"

# Run all modules in one eunit call (single compile)
MODLIST=$(echo $MODULES | tr ' ' ',')
if ./scripts/run_eunit.sh --module="$MODLIST" 2>&1 | grep -q "Error running tests"; then
    echo "Doctests failed. Run modules individually to isolate."
    exit 1
fi
echo "All doctests passed ($(echo $MODULES | wc -w) modules)."
