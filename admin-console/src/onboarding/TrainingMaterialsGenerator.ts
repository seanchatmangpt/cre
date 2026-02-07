/**
 * Training Materials Generator
 * Generates and curates training materials based on user configuration
 */

import {
  TrainingMaterial,
  TrainingCurriculum,
  SetupConfiguration,
  User,
  TrainingMetrics,
} from './types';

interface ContentLibraryItem {
  id: string;
  title: string;
  category: 'video' | 'article' | 'tutorial' | 'documentation';
  contentUrl: string;
  estimatedMinutes: number;
  difficulty: 'beginner' | 'intermediate' | 'advanced';
  tags: string[];
  prerequisites?: string[];
  relevantUseCases?: string[];
}

export class TrainingMaterialsGenerator {
  private contentLibrary: ContentLibraryItem[] = [];
  private curriculums: Map<string, TrainingCurriculum> = new Map();
  private metrics: TrainingMetrics = {
    startedCount: 0,
    completedCount: 0,
    completionRate: 0,
    averageTimePerMaterial: 0,
    mostAccessedMaterials: [],
    difficulty_distribution: {
      beginner: 0,
      intermediate: 0,
      advanced: 0,
    },
  };

  constructor() {
    this.initializeContentLibrary();
  }

  /**
   * Initialize content library with default materials
   */
  private initializeContentLibrary(): void {
    const library: ContentLibraryItem[] = [
      // Beginner Materials
      {
        id: 'intro-getting-started',
        title: 'Getting Started Guide',
        category: 'article',
        contentUrl: '/training/getting-started',
        estimatedMinutes: 15,
        difficulty: 'beginner',
        tags: ['onboarding', 'basics', 'getting-started'],
        relevantUseCases: ['all'],
      },
      {
        id: 'intro-dashboard-tour',
        title: 'Dashboard Tour',
        category: 'video',
        contentUrl: '/training/dashboard-tour',
        estimatedMinutes: 10,
        difficulty: 'beginner',
        tags: ['dashboard', 'navigation', 'ui'],
        relevantUseCases: ['all'],
      },
      {
        id: 'intro-account-setup',
        title: 'Account Setup',
        category: 'tutorial',
        contentUrl: '/training/account-setup',
        estimatedMinutes: 20,
        difficulty: 'beginner',
        tags: ['account', 'profile', 'settings'],
        relevantUseCases: ['all'],
      },
      {
        id: 'intro-basic-concepts',
        title: 'Basic Concepts',
        category: 'article',
        contentUrl: '/training/basic-concepts',
        estimatedMinutes: 25,
        difficulty: 'beginner',
        tags: ['concepts', 'terminology', 'fundamentals'],
        relevantUseCases: ['all'],
      },

      // Intermediate Materials
      {
        id: 'inter-user-management',
        title: 'User Management',
        category: 'tutorial',
        contentUrl: '/training/user-management',
        estimatedMinutes: 30,
        difficulty: 'intermediate',
        tags: ['users', 'permissions', 'administration'],
        prerequisites: ['intro-account-setup'],
        relevantUseCases: ['team_collaboration', 'enterprise'],
      },
      {
        id: 'inter-integrations',
        title: 'Setting Up Integrations',
        category: 'tutorial',
        contentUrl: '/training/integrations',
        estimatedMinutes: 40,
        difficulty: 'intermediate',
        tags: ['integrations', 'api', 'webhooks'],
        prerequisites: ['intro-basic-concepts'],
        relevantUseCases: ['enterprise', 'api_integration'],
      },
      {
        id: 'inter-workflows',
        title: 'Creating Workflows',
        category: 'video',
        contentUrl: '/training/workflows',
        estimatedMinutes: 35,
        difficulty: 'intermediate',
        tags: ['workflows', 'automation', 'processes'],
        prerequisites: ['intro-dashboard-tour'],
        relevantUseCases: ['automation', 'workflow_management'],
      },
      {
        id: 'inter-reporting',
        title: 'Reporting and Analytics',
        category: 'article',
        contentUrl: '/training/reporting',
        estimatedMinutes: 25,
        difficulty: 'intermediate',
        tags: ['reporting', 'analytics', 'insights'],
        prerequisites: ['intro-basic-concepts'],
        relevantUseCases: ['analytics', 'enterprise'],
      },

      // Advanced Materials
      {
        id: 'adv-api-development',
        title: 'API Development Guide',
        category: 'documentation',
        contentUrl: '/training/api-development',
        estimatedMinutes: 60,
        difficulty: 'advanced',
        tags: ['api', 'development', 'coding'],
        prerequisites: ['inter-integrations'],
        relevantUseCases: ['api_integration', 'enterprise'],
      },
      {
        id: 'adv-custom-extensions',
        title: 'Building Custom Extensions',
        category: 'tutorial',
        contentUrl: '/training/custom-extensions',
        estimatedMinutes: 90,
        difficulty: 'advanced',
        tags: ['extensions', 'customization', 'development'],
        prerequisites: ['inter-workflows'],
        relevantUseCases: ['enterprise', 'customization'],
      },
      {
        id: 'adv-performance-tuning',
        title: 'Performance Tuning',
        category: 'documentation',
        contentUrl: '/training/performance-tuning',
        estimatedMinutes: 45,
        difficulty: 'advanced',
        tags: ['performance', 'optimization', 'scaling'],
        relevantUseCases: ['enterprise', 'high_volume'],
      },
      {
        id: 'adv-security-best-practices',
        title: 'Security Best Practices',
        category: 'article',
        contentUrl: '/training/security',
        estimatedMinutes: 35,
        difficulty: 'advanced',
        tags: ['security', 'best-practices', 'compliance'],
        relevantUseCases: ['enterprise', 'compliance'],
      },
    ];

    this.contentLibrary = library;
  }

  /**
   * Generate curriculum based on user configuration
   */
  async generateCurriculum(user: User, config: SetupConfiguration): Promise<TrainingCurriculum> {
    const selectedMaterials = this.selectMaterials(config);
    const materials = selectedMaterials.map((item) => ({
      id: item.id,
      title: item.title,
      category: item.category,
      contentUrl: item.contentUrl,
      estimatedMinutes: item.estimatedMinutes,
      difficulty: item.difficulty,
      tags: item.tags,
      completed: false,
    } as TrainingMaterial));

    const curriculum: TrainingCurriculum = {
      userId: user.id,
      materials,
      completedCount: 0,
      totalMinutes: materials.reduce((sum, m) => sum + m.estimatedMinutes, 0),
      completionPercentage: 0,
      nextRecommendedMaterial: materials.length > 0 ? materials[0] : undefined,
    };

    this.curriculums.set(user.id, curriculum);
    this.metrics.startedCount += 1;

    return curriculum;
  }

  /**
   * Select appropriate materials based on configuration
   */
  private selectMaterials(config: SetupConfiguration): ContentLibraryItem[] {
    const selected: ContentLibraryItem[] = [];
    const selectedIds = new Set<string>();

    // Always include beginner materials
    const beginnerMaterials = this.contentLibrary.filter((m) => m.difficulty === 'beginner');
    beginnerMaterials.forEach((m) => {
      selected.push(m);
      selectedIds.add(m.id);
    });

    // Select intermediate materials based on use case
    const useCasePreference = config.primaryUseCase || 'all';
    const intermediateMaterials = this.contentLibrary.filter(
      (m) =>
        m.difficulty === 'intermediate' &&
        !selectedIds.has(m.id) &&
        m.relevantUseCases?.includes(useCasePreference)
    );

    // Add top 2-3 intermediate materials
    intermediateMaterials.slice(0, 3).forEach((m) => {
      selected.push(m);
      selectedIds.add(m.id);
    });

    // For enterprise/advanced use cases, add advanced materials
    if (
      config.organizationSize &&
      config.organizationSize > 100 &&
      (useCasePreference === 'enterprise' || useCasePreference === 'api_integration')
    ) {
      const advancedMaterials = this.contentLibrary.filter(
        (m) =>
          m.difficulty === 'advanced' &&
          !selectedIds.has(m.id) &&
          m.relevantUseCases?.includes(useCasePreference)
      );

      advancedMaterials.slice(0, 2).forEach((m) => {
        selected.push(m);
        selectedIds.add(m.id);
      });
    }

    return this.orderMaterials(selected);
  }

  /**
   * Order materials by recommended learning sequence
   */
  private orderMaterials(materials: ContentLibraryItem[]): ContentLibraryItem[] {
    // Sort by difficulty first, then by estimated time
    return materials.sort((a, b) => {
      const difficultyOrder = { beginner: 0, intermediate: 1, advanced: 2 };
      const aDiff = difficultyOrder[a.difficulty];
      const bDiff = difficultyOrder[b.difficulty];

      if (aDiff !== bDiff) {
        return aDiff - bDiff;
      }

      return a.estimatedMinutes - b.estimatedMinutes;
    });
  }

  /**
   * Mark material as completed
   */
  async markMaterialCompleted(userId: string, materialId: string): Promise<TrainingCurriculum> {
    const curriculum = this.curriculums.get(userId);
    if (!curriculum) {
      throw new Error(`Curriculum not found for user ${userId}`);
    }

    const material = curriculum.materials.find((m) => m.id === materialId);
    if (!material) {
      throw new Error(`Material ${materialId} not found in curriculum`);
    }

    if (!material.completed) {
      material.completed = true;
      material.completedAt = new Date();
      curriculum.completedCount += 1;
      curriculum.completionPercentage = (curriculum.completedCount / curriculum.materials.length) * 100;

      // Find next uncompleted material
      curriculum.nextRecommendedMaterial = curriculum.materials.find((m) => !m.completed);

      this.curriculums.set(userId, curriculum);
      this.metrics.completedCount += 1;
      this.updateMetrics();
    }

    return curriculum;
  }

  /**
   * Get curriculum for a user
   */
  getCurriculum(userId: string): TrainingCurriculum | undefined {
    return this.curriculums.get(userId);
  }

  /**
   * Get recommended next material
   */
  getRecommendedNextMaterial(userId: string): TrainingMaterial | undefined {
    const curriculum = this.curriculums.get(userId);
    if (!curriculum) {
      return undefined;
    }

    // Find first incomplete material
    const nextIncomplete = curriculum.materials.find((m) => !m.completed);
    if (nextIncomplete) {
      return nextIncomplete;
    }

    // If all completed, return undefined
    return undefined;
  }

  /**
   * Get materials by category
   */
  getMaterialsByCategory(
    userId: string,
    category: 'video' | 'article' | 'tutorial' | 'documentation'
  ): TrainingMaterial[] {
    const curriculum = this.curriculums.get(userId);
    if (!curriculum) {
      return [];
    }

    return curriculum.materials.filter((m) => m.category === category);
  }

  /**
   * Update training metrics
   */
  private updateMetrics(): void {
    const totalCurriculums = this.curriculums.size;
    const completedCurriculums = Array.from(this.curriculums.values()).filter(
      (c) => c.completionPercentage === 100
    ).length;

    this.metrics.completionRate = totalCurriculums > 0 ? (completedCurriculums / totalCurriculums) * 100 : 0;

    // Calculate average time and difficulty distribution
    let totalTime = 0;
    let totalMaterials = 0;
    const difficultyDist = { beginner: 0, intermediate: 0, advanced: 0 };

    this.curriculums.forEach((curriculum) => {
      curriculum.materials.forEach((material) => {
        totalTime += material.estimatedMinutes;
        totalMaterials += 1;
        difficultyDist[material.difficulty] += 1;
      });
    });

    this.metrics.averageTimePerMaterial = totalMaterials > 0 ? totalTime / totalMaterials : 0;
    this.metrics.difficulty_distribution = difficultyDist;

    // Find most accessed materials
    const materialAccessCounts = new Map<string, number>();
    this.curriculums.forEach((curriculum) => {
      curriculum.materials.forEach((material) => {
        materialAccessCounts.set(material.id, (materialAccessCounts.get(material.id) || 0) + 1);
      });
    });

    this.metrics.mostAccessedMaterials = Array.from(materialAccessCounts.entries())
      .sort((a, b) => b[1] - a[1])
      .slice(0, 10)
      .map(([id]) => id);
  }

  /**
   * Get training metrics
   */
  getMetrics(): TrainingMetrics {
    return { ...this.metrics };
  }

  /**
   * Get content library (for management)
   */
  getContentLibrary(): ContentLibraryItem[] {
    return [...this.contentLibrary];
  }

  /**
   * Add custom content to library
   */
  addCustomContent(item: ContentLibraryItem): void {
    if (this.contentLibrary.find((m) => m.id === item.id)) {
      throw new Error(`Content ${item.id} already exists`);
    }
    this.contentLibrary.push(item);
  }

  /**
   * Clear cache (for testing)
   */
  clearCache(): void {
    this.curriculums.clear();
    this.metrics = {
      startedCount: 0,
      completedCount: 0,
      completionRate: 0,
      averageTimePerMaterial: 0,
      mostAccessedMaterials: [],
      difficulty_distribution: {
        beginner: 0,
        intermediate: 0,
        advanced: 0,
      },
    };
  }
}
