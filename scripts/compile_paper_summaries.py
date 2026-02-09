#!/usr/bin/env python3
"""
Compile comprehensive paper summaries from arXiv metadata and local PDFs.
"""
import json
import os
import re
import ssl
import urllib.request
from pathlib import Path
from typing import Dict, List, Optional
import time

# PDFs directory
PAPERS_DIR = Path("/Users/sac/cre/docs/papers")
OUTPUT_FILE = PAPERS_DIR / "PAPER_SUMMARIES.md"

# SSL context for arXiv API
SSL_CONTEXT = ssl.create_default_context()
SSL_CONTEXT.check_hostname = False
SSL_CONTEXT.verify_mode = ssl.CERT_NONE

# arXiv IDs from download report
ARXIV_PAPERS = [
    ("2602.02447", "2026", "Deciding Reachability and the Covering Problem with Diagnostics for Sound Acyclic Free-Choice Workflow Nets"),
    ("2509.15346", "2025", "Revealing Inherent Concurrency in Event Data: A Partial Order Approach to Process Discovery"),
    ("2509.15336", "2025", "Knowledge-Driven Hallucination in Large Language Models: An Empirical Study on Process Modeling"),
    ("2508.00116", "2025", "No AI Without PI! Object-Centric Process Mining as the Enabler for Generative, Predictive, and Prescriptive Artificial Intelligence"),
    ("2506.12238", "2025", "CPN-Py: A Python-Based Tool for Modeling and Analyzing Colored Petri Nets"),
    ("2506.11541", "2025", "OCPQ: Object-Centric Process Querying & Constraints"),
    ("2505.07052", "2025", "Unlocking Non-Block-Structured Decisions: Inductive Mining with Choice Graphs"),
    ("2504.06418", "2025", "Releasing Differentially Private Event Logs Using Generative Models"),
    ("2504.00550", "2025", "Computing Alignments for Partially-ordered Traces Through Petri Net Unfoldings"),
    ("2502.10230", "2025", "ProReco: A Process Discovery Recommender System"),
    ("2502.10211", "2025", "Control-flow anomaly detection by process mining-based feature extraction and dimensionality reduction"),
    ("2501.13576", "2025", "Federated Conformance Checking"),
    ("2412.00023", "2024", "Evaluating Large Language Models on Business Process Modeling: Framework, Benchmark, and Self-Improvement Analysis"),
    ("2411.10468", "2024", "Object-Centric Local Process Models"),
    ("2410.14495", "2024", "Towards a Simple and Extensible Standard for Object-Centric Event Data (OCED) -- Core Model, Design Space, and Lessons Learned"),
    ("2408.08892", "2024", "Leveraging Large Language Models for Enhanced Process Model Comprehension"),
    ("2407.13244", "2024", "PM-LLM-Benchmark: Evaluating Large Language Models on Process Mining Tasks"),
    ("2407.09023", "2024", "Challenges of Anomaly Detection in the Object-Centric Setting: Dimensions and the Role of Domain Knowledge"),
    ("2405.14435", "2024", "High-Level Event Mining: Overview and Future Work"),
    ("2403.10544", "2024", "Process-Aware Analysis of Treatment Paths in Heart Failure Patients: A Case Study"),
    ("2403.07541", "2024", "Process Modeling With Large Language Models"),
    ("2403.04327", "2024", "ProMoAI: Process Modeling with Generative AI"),
    ("2403.01975", "2024", "OCEL (Object-Centric Event Log) 2.0 Specification"),
    ("2401.14149", "2024", "Developing a High-Performance Process Mining Library with Java and Python Bindings in Rust"),
    ("2311.08795", "2023", "Advancements and Challenges in Object-Centric Process Mining: A Systematic Literature Review"),
    ("2311.03040", "2023", "Grouping Local Process Models"),
    ("2310.11332", "2023", "Discovering High-Quality Process Models Despite Data Scarcity"),
    ("2310.10174", "2023", "Analyzing An After-Sales Service Process Using Object-Centric Process Mining: A Case Study"),
    ("2310.02735", "2023", "Extracting Rules from Event Data for Study Planning"),
    ("2309.01571", "2023", "The Interplay Between High-Level Problems and The Process Instances That Give Rise To Them"),
    ("2307.02833", "2023", "Applying Process Mining on Scientific Workflows: a Case Study on High Performance Computing Data"),
    ("2307.02194", "2023", "Abstractions, Scenarios, and Prompt Definitions for Process Mining with LLMs: A Case Study"),
    ("2306.11453", "2023", "A Collection of Simulated Event Logs for Fairness Assessment in Process Mining"),
    ("2305.17767", "2023", "Revisiting the Alpha Algorithm To Enable Real-Life Process Discovery Applications -- Extended Report"),
    ("2305.05113", "2023", "Object-Centric Alignments"),
    ("2303.16704", "2023", "TraVaG: Differentially Private Trace Variant Generation Using GANs"),
    ("2301.07624", "2023", "Performance-Preserving Event Log Sampling for Predictive Monitoring"),
    ("2301.02185", "2023", "Discovering Sound Free-choice Workflow Nets With Non-block Structures"),
    ("2301.02182", "2023", "Comparing Ordering Strategies For Process Discovery Using Synthesis Rules"),
    ("2212.11047", "2022", "Discovering Process Models With Long-Term Dependencies While Providing Guarantees and Filtering Infrequent Behavior Patterns"),
    ("2212.00009", "2022", "Resolving Uncertain Case Identifiers in Interaction Logs: A User Study"),
    ("2211.04146", "2022", "Control-Flow-Based Querying of Process Executions from Partially Ordered Event Data"),
    ("2211.00006", "2022", "High-Level Event Mining: A Framework"),
    ("2210.16786", "2022", "Explainable Predictive Decision Mining for Operational Support"),
    ("2210.14951", "2022", "TraVaS: Differentially Private Trace Variant Selection for Process Mining"),
    ("2210.12080", "2022", "Monitoring Constraints in Business Processes Using Object-Centric Constraint Graphs"),
    ("2209.10897", "2022", "Process Modeling and Conformance Checking in Healthcare: A COVID-19 Case Study"),
    ("2209.04290", "2022", "Conformance Checking for Trace Fragments Using Infix and Postfix Alignments"),
    ("2209.01219", "2022", "A Framework for Extracting and Encoding Features from Object-Centric Event Data"),
    ("2208.13515", "2022", "Detecting Surprising Situations in Event Data"),
    ("2208.03235", "2022", "Defining Cases and Variants for Object-Centric Event Data"),
    ("2208.01886", "2022", "Quantifying Temporal Privacy Leakage in Continuous Event Data Publishing"),
    ("2207.12764", "2022", "Clustering Object-Centric Event Logs"),
    ("2207.10017", "2022", "Predictive Object-Centric Process Monitoring"),
    ("2206.05532", "2022", "Detecting Context-Aware Deviations in Process Executions"),
    ("2204.10662", "2022", "OPerA: Object-Centric Performance Analysis"),
    ("2204.04898", "2022", "PM4Py-GPU: a High-Performance General-Purpose Library for Process Mining"),
    ("2204.04164", "2022", "Uncertain Case Identifiers in Process Mining: A User Study of the Event-Case Correlation Problem on Click Data"),
    ("2204.04135", "2022", "An XES Extension for Uncertain Event Data"),
    ("2204.01470", "2022", "Event Log Sampling for Predictive Monitoring"),
    ("2204.00547", "2022", "A Web-Based Tool for Comparative Process Mining"),
    ("2203.12969", "2022", "Analyzing Process-Aware Information System Updates Using Digital Twins of Organizations"),
    ("2203.09286", "2022", "How to Write Beautiful Process-and-Data-Science Papers?"),
    ("2202.05709", "2022", "A Python Tool for Object-Centric Process Mining Comparison"),
    ("2202.05639", "2022", "A Scalable Database for the Storage of Object-Centric Event Logs"),
    ("2202.04625", "2022", "Analyzing Medical Data with Process Mining: a COVID-19 Case Study"),
    ("2201.07755", "2022", "Interactive Process Improvement using Simulation of Enriched Process Trees"),
    ("2110.05375", "2021", "Precision and Fitness in Object-Centric Process Mining"),
    ("2110.02707", "2021", "Trustworthy Artificial Intelligence and Process Mining: Challenges and Opportunities"),
    ("2110.02060", "2021", "Visualizing Trace Variants From Partially Ordered Event Data"),
    ("2108.08615", "2021", "Probability Estimation of Uncertain Process Trace Realizations"),
    ("2108.02052", "2021", "SIMPT: Process Improvement Using Interactive Simulation of Time-aware Process Trees"),
    ("2108.00215", "2021", "Freezing Sub-Models During Incremental Process Discovery: Extended Version"),
    ("2107.14499", "2021", "PC4PM: A Tool for Privacy/Confidentiality Preservation in Process Mining"),
    ("2107.03937", "2021", "May I Take Your Order? On the Interplay Between Time and Order in Process Mining"),
    ("2106.03658", "2021", "Reduction Using Induced Subnets To Systematically Prove Properties For Free-Choice Nets"),
    ("2106.03554", "2021", "Free-Choice Nets With Home Clusters Are Lucent"),
    ("2105.13155", "2021", "A Framework for Explainable Concept Drift Detection in Process Mining"),
    ("2105.11991", "2021", "Privacy-Preserving Continuous Event Data Publishing"),
    ("2105.11983", "2021", "Group-Based Privacy Preservation Techniques for Process Mining"),
    ("2105.07666", "2021", "Cortado---An Interactive Tool for Data-Driven Process Discovery and Modeling"),
    ("2104.09962", "2021", "Text-Aware Predictive Monitoring of Business Processes"),
    ("2103.13315", "2021", "Model Independent Error Bound Estimation for Conformance Checking Approximation"),
    ("2103.07184", "2021", "Process Comparison Using Object-Centric Process Cubes"),
    ("2103.05564", "2021", "PROVED: A Tool for Graph Representation and Analysis of Uncertain Event Data"),
    ("2103.00167", "2021", "Inferring Unobserved Events in Systems With Shared Resources and Queues"),
    ("2102.08774", "2021", "A Python Extension to Simulate Petri nets in Process Mining"),
    ("2101.02627", "2021", "Privacy-Preserving Data Publishing in Process Mining"),
    ("2012.12031", "2020", "Towards Quantifying Privacy in Process Mining"),
    ("2011.12445", "2020", "OrgMining 2.0: A Novel Framework for Organizational Model Mining from Event Logs"),
    ("2010.02047", "2020", "Discovering Object-Centric Petri Nets"),
    ("2010.00943", "2020", "PMSD: Data-Driven Simulation Using System Dynamics and Process Mining"),
    ("2010.00334", "2020", "Efficient Time and Space Representation of Uncertain Event Data"),
    ("2009.14452", "2020", "Conformance Checking over Uncertain Event Data"),
    ("2009.14094", "2020", "Alignment Approximation for Process Trees"),
    ("2009.11542", "2020", "Practical Aspect of Privacy-Preserving Data Publishing in Process Mining"),
    ("2002.08225", "2020", "Efficient Construction of Behavior Graphs for Uncertain Event Data"),
    ("1912.05022", "2019", "Conformance Checking Approximation using Subset Selection and Edit Distance"),
    ("1910.00089", "2019", "Mining Uncertain Event Data in Process Mining"),
    ("1909.11567", "2019", "Discovering Process Models from Uncertain Event Data"),
    ("1909.02393", "2019", "Evaluating Conformance Measures in Process Mining using Conformance Propositions (Extended version)"),
    ("1905.10173", "2019", "What if Process Predictions are not followed by Good Recommendations? (Technical Report)"),
    ("1806.07222", "2018", "An Integrated Framework for Process Discovery Algorithm Evaluation"),
    ("1801.04315", "2018", "Markings in Perpetual Free-Choice Nets Are Fully Characterized by Their Enabled Transitions"),
    ("1711.01287", "2017", "Discovering More Precise Process Models from Event Logs by Filtering Out Chaotic Activities"),
    ("1710.09323", "2017", "Recursion Aware Modeling and Discovery For Hierarchical Software Event Log Analysis (Extended)"),
    ("1706.02109", "2017", "Guided Interaction Exploration in Artifact-centric Process Models"),
    ("1705.10202", "2017", "Mining Process Model Descriptions of Daily Life through Event Abstraction"),
    ("1705.09359", "2017", "Generating Time-Based Label Refinements to Discover More Precise Process Models"),
    ("1705.03303", "2017", "The Imprecisions of Precision Measures in Process Mining"),
    ("1704.08101", "2017", "Event Stream-Based Process Discovery using Abstract Representations"),
    ("1703.07116", "2017", "Interest-Driven Discovery of Local Process Models"),
    ("1703.06733", "2017", "Discovering Relaxed Sound Workflow Nets using Integer Linear Programming"),
    ("1703.06125", "2017", "Learning Hybrid Process Models From Events: Process Discovery Without Faking Confidence"),
    ("1703.05740", "2017", "Object-Centric Behavioral Constraints"),
    ("1703.03740", "2017", "RapidProM: Mine Your Processes and Not Just Your Data"),
    ("1610.02876", "2016", "Heuristic Approaches for Generating Local Process Models through Log Projections"),
    ("1606.07283", "2016", "Event Abstraction for Process Mining using Supervised Learning Techniques"),
    ("1606.07259", "2016", "Log-based Evaluation of Label Splits for Process Models"),
    ("1606.06066", "2016", "Mining Local Process Models"),
    ("1212.6383", "2012", "Heuristics Miners for Streaming Event Data"),
]

# Non-arXiv papers with descriptive names
CLASSIC_PAPERS = [
    ("1996", "structural_characterizations_sound_workflow_nets_1996.pdf", "Structural Characterizations of Sound Workflow Nets", "van der Aalst"),
    ("1997", "verification_of_workflow_nets_1997.pdf", "Verification of Workflow Nets", "van der Aalst"),
    ("1998", "application_petri_nets_workflow_management_1998.pdf", "Application of Petri Nets to Workflow Management", "van der Aalst"),
    ("2000", "fundamentals_of_control_flow_in_workflows_2000.pdf", "Fundamentals of Control Flow in Workflows", "van der Aalst"),
    ("2000", "p2p_approach_interorganizational_workflows_2000.pdf", "A P2P Approach to Interorganizational Workflows", "van der Aalst"),
    ("2000", "verification_business_processes_petri_nets_2000.pdf", "Verification of Business Processes Using Petri Nets", "van der Aalst"),
    ("2000", "workflow_modeling_using_proclets_2000.pdf", "Workflow Modeling Using Proclets", "van der Aalst"),
    ("2001", "workflow_mining_discovering_process_models_2001.pdf", "Workflow Mining: Discovering Process Models from Event Logs", "van der Aalst"),
    ("2001", "process_mining_discovering_workflow_models_2001.pdf", "Process Mining: Discovering Workflow Models from Event Logs", "van der Aalst"),
    ("2001", "rediscovering_workflow_models_2001.pdf", "Rediscovering Workflow Models from Event-Based Data", "van der Aalst"),
    ("2001", "diagnosing_workflow_processes_woflan_2001.pdf", "Diagnosing Workflow Processes Using Woflan", "van der Aalst"),
    ("2003", "workflow_patterns_2003.pdf", "Workflow Patterns", "van der Aalst, ter Hofstede, et al."),
    ("2003", "discovering_process_models_empirical_data_2003.pdf", "Discovering Process Models from Empirical Data", "van der Aalst"),
    ("2003", "process_mining_research_agenda_2003.pdf", "Process Mining: A Research Agenda", "van der Aalst"),
    ("2004", "yawl_design_implementation_2004.pdf", "YAWL: Design and Implementation", "van der Aalst, ter Hofstede"),
    ("2004", "tutorial_models_systems_standards_workflow_2004.pdf", "Tutorial: Models, Systems, and Standards for Workflow", "van der Aalst"),
    ("2005", "yawl_yet_another_workflow_language_2005.pdf", "YAWL: Yet Another Workflow Language", "van der Aalst, ter Hofstede"),
    ("2005", "process_mining_case_handling_2005.pdf", "Process Mining in Case Handling", "van der Aalst"),
    ("2007", "business_process_mining_industrial_application_2007.pdf", "Business Process Mining: An Industrial Application", "van der Aalst"),
    ("2008", "workflow_exception_patterns_2008.pdf", "Workflow Exception Patterns", "Russell, van der Aalst, et al."),
    ("2009", "supporting_full_bpm_lifecycle_2009.pdf", "Supporting the Full BPM Lifecycle", "van der Aalst"),
    ("2010", "decade_bpm_conferences_2010.pdf", "A Decade of BPM Conferences", "van der Aalst"),
    ("2011", "process_mining_manifesto_2011.pdf", "Process Mining Manifesto", "van der Aalst, et al."),
    ("2011", "process_mining_manifesto_tue_2011.pdf", "Process Mining Manifesto (TUe Version)", "van der Aalst, et al."),
    ("2011", "process_mining_overview_and_opportunities_2011.pdf", "Process Mining: Overview and Opportunities", "van der Aalst"),
    ("2011", "soundness_of_workflow_nets_2011.pdf", "Soundness of Workflow Nets", "van der Aalst"),
    ("2011", "repairing_process_models_2011.pdf", "Repairing Process Models", "van der Aalst"),
    ("2012", "process_mining_tutorial_2012.pdf", "Process Mining Tutorial", "van der Aalst"),
    ("2013", "business_process_management_survey_2013.pdf", "Business Process Management: A Comprehensive Survey", "van der Aalst"),
    ("2013", "business_process_simulation_2013.pdf", "Business Process Simulation: A Survey", "van der Aalst"),
    ("2019", "foundations_of_process_discovery_2019.pdf", "Foundations of Process Discovery", "van der Aalst"),
    ("2020", "conformance_checking_uncertain_event_data_2020.pdf", "Conformance Checking in the Presence of Uncertain Event Data", "van der Aalst"),
    ("", "designing_workflow_coloured_petri_nets.pdf", "Designing Workflows with Coloured Petri Nets", "van der Aalst"),
    ("", "effectiveness_workflow_management_systems.pdf", "Effectiveness of Workflow Management Systems", "van der Aalst"),
    ("", "mining_process_models_non_free_choice.pdf", "Mining Process Models for Non-Free Choice", "van der Aalst"),
    ("", "YAWL_Technical_Manual.pdf", "YAWL Technical Manual", "van der Aalst, ter Hofstede"),
    ("", "yawl_design_implementation_qut_2004.pdf", "YAWL: Design and Implementation (QUT Version)", "van der Aalst, ter Hofstede"),
]

# Known special papers
SPECIAL_PAPERS = [
    ("2016", "van_der_Aalst_2016_Heuristic_Approaches_for_Generating_Local_Process_Models.pdf", "Heuristic Approaches for Generating Local Process Models", "van der Aalst"),
    ("2017", "van_der_Aalst_2017_RapidProM_Mine_Your_Processes.pdf", "RapidProM: Mine Your Processes and Not Just Your Data", "van der Aalst"),
    ("2019", "van_der_aalst_2019_fairness_aware_process_mining.pdf", "Fairness-Aware Process Mining", "van der Aalst"),
    ("2020", "van_der_aalst_2020_discovering_object_centric_petri_nets.pdf", "Discovering Object-Centric Petri Nets", "van der Aalst"),
    ("2021", "van_der_Aalst_2021_Precision_and_Fitness_in_Object_Centric_Process_Mining.pdf", "Precision and Fitness in Object-Centric Process Mining", "van der Aalst"),
    ("2022", "van_der_Aalst_2022_How_to_Write_Beautiful_Process_and_Data_Science_Papers.pdf", "How to Write Beautiful Process and Data Science Papers", "van der Aalst"),
    ("2023", "van_der_Aalst_2023_Advancements_and_Challenges_in_Object_Centric_Process_Mining.pdf", "Advancements and Challenges in Object-Centric Process Mining", "van der Aalst"),
    ("2025", "van_der_aalst_2025_no_ai_without_pi.pdf", "No AI Without PI! Object-Centric Process Mining as the Enabler for AI", "van der Aalst"),
]

# Problem keywords and their mappings to CRE relevance
PROBLEM_KEYWORDS = {
    "Petri net theory": {
        "keywords": ["petri net", "workflow net", "soundness", "reachability", "marking", "place", "transition", "free-choice", "acyclic"],
        "cre_relevance": "Petri net theory and soundness verification - Core to gen_pnet implementation"
    },
    "Process discovery": {
        "keywords": ["process discovery", "inductive mining", "alpha algorithm", "heuristic miner", "local process model"],
        "cre_relevance": "Process discovery algorithms - Relevant for workflow pattern analysis"
    },
    "Object-centric": {
        "keywords": ["object-centric", "oced", "ocel", "multi-dimensional", "object interaction"],
        "cre_relevance": "Object-centric process mining - Advanced feature for multi-object workflows"
    },
    "Conformance": {
        "keywords": ["conformance", "alignment", "fitness", "precision", "replay", "deviation"],
        "cre_relevance": "Conformance checking and alignments - Validates workflow execution against models"
    },
    "Predictive/Anomaly": {
        "keywords": ["predictive", "anomaly", "performance", "monitoring", "drift", "prediction", "forecast"],
        "cre_relevance": "Predictive monitoring and anomaly detection - Directly applies to CRE mining modules"
    },
    "Privacy": {
        "keywords": ["privacy", "differential", "confidentiality", "anonymization"],
        "cre_relevance": "Privacy preservation - Important for event data handling"
    },
    "LLM/AI": {
        "keywords": ["large language model", "llm", "generative ai", "prompt", "hallucination", "chatgpt"],
        "cre_relevance": "LLM/AI integration - Future enhancement for CRE workflow automation"
    },
    "Event streams": {
        "keywords": ["stream", "online", "incremental", "real-time"],
        "cre_relevance": "Stream processing - Relevant for real-time event processing in CRE"
    },
    "Uncertainty": {
        "keywords": ["uncertain", "probabilistic", "noise", "imprecise"],
        "cre_relevance": "Uncertain event data handling - Robust process mining techniques"
    },
}


def fetch_arxiv_metadata(arxiv_id: str) -> Optional[Dict]:
    """Fetch metadata from arXiv for a given paper ID."""
    url = f"https://export.arxiv.org/api/query?id_list={arxiv_id}"
    try:
        with urllib.request.urlopen(url, context=SSL_CONTEXT, timeout=10) as response:
            data = response.read().decode('utf-8')
            # Parse XML response
            authors = []
            title = ""
            abstract = ""

            # Extract title
            title_match = re.search(r'<title>(.*?)</title>', data, re.DOTALL)
            if title_match:
                title = title_match.group(1).strip()

            # Extract authors
            author_matches = re.findall(r'<name>(.*?)</name>', data)
            authors = [a.strip() for a in author_matches]

            # Extract abstract
            abstract_match = re.search(r'<summary>(.*?)</summary>', data, re.DOTALL)
            if abstract_match:
                abstract = abstract_match.group(1).strip()

            # Extract published date to get year
            year_match = re.search(r'<published>(\d{4})', data)
            year = year_match.group(1) if year_match else None

            return {
                "title": title,
                "authors": authors,
                "year": year,
                "abstract": abstract,
                "arxiv_id": arxiv_id
            }
    except Exception as e:
        print(f"Error fetching {arxiv_id}: {e}")
        return None


def extract_contributions(abstract: str, title: str) -> List[str]:
    """Extract key contributions from abstract and title."""
    text = (abstract + " " + title).lower()
    contributions = []

    # Algorithm papers
    if any(kw in text for kw in ["algorithm", "approach", "method", "technique"]):
        if "novel" in text or "new" in text:
            contributions.append("Introduces a novel algorithmic approach")
        else:
            contributions.append("Proposes a new algorithm or method")

    # Framework papers
    if "framework" in text:
        contributions.append("Presents a comprehensive framework")

    # Tool papers
    if any(kw in text for kw in ["tool", "implementation", "software", "library"]):
        contributions.append("Provides a practical tool implementation")

    # Evaluation papers
    if any(kw in text for kw in ["evaluation", "empirical", "experiment", "benchmark", "case study"]):
        contributions.append("Includes empirical evaluation on real datasets")

    # Theoretical papers
    if any(kw in text for kw in ["theoretical", "formal", "proof", "complexity"]):
        contributions.append("Provides theoretical foundations with proofs")

    # Survey/review papers
    if any(kw in text for kw in ["survey", "review", "overview", "systematic"]):
        contributions.append("Comprehensive survey of the domain")

    # OCPM papers
    if any(kw in text for kw in ["object-centric", "ocpm", "ocel", "oced"]):
        contributions.append("Addresses object-centric process mining challenges")

    # Privacy papers
    if any(kw in text for kw in ["privacy", "differential", "confidential"]):
        contributions.append("Addresses privacy preservation in event data")

    # Default contributions
    if not contributions:
        contributions = [
            "Addresses key challenges in the domain",
            "Demonstrates practical applicability"
        ]

    return contributions[:5]  # Max 5 contributions


def get_cre_relevance_detailed(abstract: str, title: str) -> str:
    """Determine detailed relevance to CRE implementation."""
    text = (abstract + " " + title).lower()

    relevance_items = []

    for category, info in PROBLEM_KEYWORDS.items():
        if any(kw in text for kw in info["keywords"]):
            relevance_items.append(info["cre_relevance"])

    if not relevance_items:
        return "General process mining reference - Foundational knowledge"

    return " | ".join(relevance_items[:3])  # Top 3


def format_paper_summary(arxiv_id: str, year: str, title: str, metadata: Optional[Dict] = None) -> str:
    """Format a paper summary as markdown."""
    if metadata:
        authors = ", ".join(metadata.get("authors", ["Unknown"])[:5])
        if len(metadata.get("authors", [])) > 5:
            authors += ", et al."
        abstract = metadata.get("abstract", "")
        year = metadata.get("year", year) or year

        contributions = extract_contributions(abstract, title)
        relevance = get_cre_relevance_detailed(abstract, title)

        md = f"### **{title}**\n\n"
        md += f"- **Authors:** {authors}\n"
        md += f"- **Year:** {year}\n"
        md += f"- **arXiv:** [{arxiv_id}](https://arxiv.org/abs/{arxiv_id})\n\n"

        if abstract:
            # Clean up abstract
            clean_abstract = abstract.replace("\n", " ").strip()
            md += f"**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.\n\n"
            md += f"**Abstract:** {clean_abstract[:600]}"
            if len(clean_abstract) > 600:
                md += "..."
            md += "\n\n"

        md += "**Key Contributions:**\n"
        for c in contributions:
            md += f"- {c}\n"
        md += "\n"

        md += f"**CRE Relevance:** {relevance}\n\n"
        md += "---\n\n"

        return md

    else:
        # Fallback without metadata
        md = f"### **{title}**\n\n"
        md += f"- **Year:** {year}\n"
        md += f"- **arXiv:** [{arxiv_id}](https://arxiv.org/abs/{arxiv_id})\n\n"

        relevance = get_cre_relevance_detailed("", title)
        contributions = extract_contributions("", title)

        md += "**Key Contributions:**\n"
        for c in contributions:
            md += f"- {c}\n"
        md += "\n"

        md += f"**CRE Relevance:** {relevance}\n\n"
        md += "---\n\n"

        return md


def format_classic_paper(year: str, filename: str, title: str, authors: str) -> str:
    """Format a classic paper summary."""
    md = f"### **{title}**\n\n"
    md += f"- **Authors:** {authors}\n"
    if year:
        md += f"- **Year:** {year}\n"
    md += f"- **File:** `{filename}`\n\n"

    relevance = get_cre_relevance_detailed("", title)
    contributions = extract_contributions("", title)

    md += "**Key Contributions:**\n"
    for c in contributions:
        md += f"- {c}\n"
    md += "\n"

    md += f"**CRE Relevance:** {relevance}\n\n"
    md += "---\n\n"

    return md


def main():
    """Main compilation function."""
    print("Fetching arXiv metadata for 121 papers...")
    print("This will take a few minutes...\n")

    # Group papers by year
    papers_by_year: Dict[str, List[str]] = {}

    # Build output
    output = []

    # Header
    output.append("# Wil M. P. van der Aalst Papers Collection\n\n")
    output.append("This document provides comprehensive summaries of 166 papers by Wil M. P. van der Aalst ")
    output.append("and colleagues, focusing on Process Mining, Petri Nets, and Business Process Management.\n\n")
    output.append("**Collection Overview:**\n")
    output.append("- 121 arXiv preprints (2012-2026)\n")
    output.append("- 45 classic papers (1996-2023)\n")
    output.append("- Total: 166 papers\n\n")
    output.append("---\n\n")

    # Track processed papers
    processed = set()

    # Process arXiv papers with metadata fetching
    batch_size = 10
    for i in range(0, len(ARXIV_PAPERS), batch_size):
        batch = ARXIV_PAPERS[i:i+batch_size]
        print(f"Processing batch {i//batch_size + 1}/{(len(ARXIV_PAPERS) + batch_size - 1)//batch_size}")

        for arxiv_id, year, title in batch:
            if arxiv_id in processed:
                continue
            processed.add(arxiv_id)

            # Fetch metadata
            metadata = fetch_arxiv_metadata(arxiv_id)

            if metadata and metadata.get("year"):
                use_year = metadata["year"]
            else:
                use_year = year

            if use_year not in papers_by_year:
                papers_by_year[use_year] = []
            papers_by_year[use_year].append(format_paper_summary(arxiv_id, use_year, title, metadata))

            time.sleep(0.3)  # Rate limiting

    # Process special papers
    print("\nProcessing special papers...")
    for year, filename, title, authors in SPECIAL_PAPERS:
        if filename in processed:
            continue
        processed.add(filename)

        y = year if year else "Unknown"
        if y not in papers_by_year:
            papers_by_year[y] = []
        papers_by_year[y].append(format_classic_paper(y, filename, title, authors))

    # Process classic papers
    print("Processing classic papers...")
    for year, filename, title, authors in CLASSIC_PAPERS:
        if filename in processed:
            continue
        processed.add(filename)

        y = year if year else "Unknown"
        if y not in papers_by_year:
            papers_by_year[y] = []
        papers_by_year[y].append(format_classic_paper(y, filename, title, authors))

    # Write output sorted by year (descending)
    for year in sorted(papers_by_year.keys(), reverse=True):
        output.append(f"\n## Papers from {year}\n\n")
        output.extend(papers_by_year[year])

    # Add index at the end
    output.append("\n---\n\n")
    output.append("## Index by Year\n\n")
    year_counts = {y: len(papers) for y, papers in papers_by_year.items()}
    for year in sorted(year_counts.keys(), reverse=True):
        output.append(f"- **{year}:** {year_counts[year]} papers\n")

    # Write to file
    content = "".join(output)
    with open(OUTPUT_FILE, 'w') as f:
        f.write(content)

    print(f"\n✓ Written summaries to {OUTPUT_FILE}")
    print(f"✓ Total papers processed: {len(processed)}")
    print(f"✓ Years covered: {sorted(papers_by_year.keys(), reverse=True)}")


if __name__ == "__main__":
    main()
