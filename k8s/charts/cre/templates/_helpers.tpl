{{/*
Expand the name of the chart.
*/}}
{{- define "cre.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Create a default fully qualified app name.
*/}}
{{- define "cre.fullname" -}}
{{- if .Values.fullnameOverride }}
{{- .Values.fullnameOverride | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- $name := default .Chart.Name .Values.nameOverride }}
{{- if contains $name .Release.Name }}
{{- .Release.Name | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Create chart name and version as used by the chart label.
*/}}
{{- define "cre.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Common labels
*/}}
{{- define "cre.labels" -}}
helm.sh/chart: {{ include "cre.chart" . }}
{{ include "cre.selectorLabels" . }}
{{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- end }}

{{/*
Selector labels
*/}}
{{- define "cre.selectorLabels" -}}
app.kubernetes.io/name: {{ include "cre.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{/*
Erlang node name
*/}}
{{- define "cre.erlangNode" -}}
{{- printf "cre@%s-%d.%s.%s.svc.cluster.local" (include "cre.fullname" .) (int) .Release.Revision .Release.Namespace "headless" }}
{{- end }}

{{/*
Mnesia data directory
*/}}
{{- define "cre.mnesiaDir" -}}
{{- default "/opt/cre/data/mnesia" .Values.config.mnesiaDir }}
{{- end }}
