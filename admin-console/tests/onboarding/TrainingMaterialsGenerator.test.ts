/**
 * Training Materials Generator Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { TrainingMaterialsGenerator } from '@/onboarding/TrainingMaterialsGenerator';
import { User, SetupConfiguration } from '@/onboarding/types';

describe('TrainingMaterialsGenerator', () => {
  let generator: TrainingMaterialsGenerator;

  const mockUser: User = {
    id: 'user-123',
    email: 'user@example.com',
    name: 'John Doe',
    organizationId: 'org-123',
    role: 'user',
    createdAt: new Date(),
  };

  const basicConfig: SetupConfiguration = {
    organizationName: 'Test Org',
    organizationSize: 50,
    department: 'Engineering',
    primaryUseCase: 'general',
    integrationType: 'none',
  };

  beforeEach(() => {
    generator = new TrainingMaterialsGenerator();
  });

  describe('curriculum generation', () => {
    it('should generate curriculum for a user', async () => {
      const curriculum = await generator.generateCurriculum(mockUser, basicConfig);

      expect(curriculum).toBeDefined();
      expect(curriculum.userId).toBe(mockUser.id);
      expect(curriculum.materials.length).toBeGreaterThan(0);
    });

    it('should include beginner materials', async () => {
      const curriculum = await generator.generateCurriculum(mockUser, basicConfig);

      const beginnerCount = curriculum.materials.filter((m) => m.difficulty === 'beginner').length;
      expect(beginnerCount).toBeGreaterThan(0);
    });

    it('should calculate total training minutes', async () => {
      const curriculum = await generator.generateCurriculum(mockUser, basicConfig);

      expect(curriculum.totalMinutes).toBeGreaterThan(0);
      const sum = curriculum.materials.reduce((acc, m) => acc + m.estimatedMinutes, 0);
      expect(curriculum.totalMinutes).toBe(sum);
    });

    it('should have initial completion percentage of 0', async () => {
      const curriculum = await generator.generateCurriculum(mockUser, basicConfig);

      expect(curriculum.completionPercentage).toBe(0);
    });

    it('should set first material as recommended', async () => {
      const curriculum = await generator.generateCurriculum(mockUser, basicConfig);

      expect(curriculum.nextRecommendedMaterial).toBeDefined();
      expect(curriculum.nextRecommendedMaterial?.id).toBe(curriculum.materials[0]?.id);
    });
  });

  describe('material selection', () => {
    it('should select materials based on organization size', async () => {
      const smallOrgConfig: SetupConfiguration = {
        ...basicConfig,
        organizationSize: 10,
      };

      const largeOrgConfig: SetupConfiguration = {
        ...basicConfig,
        organizationSize: 1000,
      };

      const smallCurriculum = await generator.generateCurriculum(mockUser, smallOrgConfig);
      const largeCurriculum = await generator.generateCurriculum(
        { ...mockUser, id: 'user-456' },
        largeOrgConfig
      );

      // Large orgs should potentially get more advanced materials
      const largeAdvancedCount = largeCurriculum.materials.filter(
        (m) => m.difficulty === 'advanced'
      ).length;
      const smallAdvancedCount = smallCurriculum.materials.filter(
        (m) => m.difficulty === 'advanced'
      ).length;

      expect(largeAdvancedCount).toBeGreaterThanOrEqual(smallAdvancedCount);
    });

    it('should select materials based on use case', async () => {
      const apiConfig: SetupConfiguration = {
        ...basicConfig,
        organizationSize: 200,
        primaryUseCase: 'api_integration',
      };

      const curriculum = await generator.generateCurriculum(mockUser, apiConfig);

      expect(curriculum.materials.length).toBeGreaterThan(0);
    });

    it('should order materials by difficulty', async () => {
      const curriculum = await generator.generateCurriculum(mockUser, basicConfig);

      const difficulties = curriculum.materials.map((m) => m.difficulty);
      const difficultyOrder = { beginner: 0, intermediate: 1, advanced: 2 };

      for (let i = 1; i < difficulties.length; i++) {
        expect(difficultyOrder[difficulties[i] as any]).toBeGreaterThanOrEqual(
          difficultyOrder[difficulties[i - 1] as any]
        );
      }
    });
  });

  describe('material tracking', () => {
    it('should mark material as completed', async () => {
      const curriculum = await generator.generateCurriculum(mockUser, basicConfig);
      const materialId = curriculum.materials[0]!.id;

      const updated = await generator.markMaterialCompleted(mockUser.id, materialId);

      const material = updated.materials.find((m) => m.id === materialId);
      expect(material?.completed).toBe(true);
      expect(material?.completedAt).toBeDefined();
    });

    it('should update completion percentage', async () => {
      const curriculum = await generator.generateCurriculum(mockUser, basicConfig);
      const materialId = curriculum.materials[0]!.id;

      const updated = await generator.markMaterialCompleted(mockUser.id, materialId);

      expect(updated.completionPercentage).toBeGreaterThan(0);
      expect(updated.completionPercentage).toBeLessThanOrEqual(100);
    });

    it('should update next recommended material', async () => {
      const curriculum = await generator.generateCurriculum(mockUser, basicConfig);
      const firstMaterialId = curriculum.materials[0]!.id;
      const secondMaterialId = curriculum.materials[1]?.id;

      const updated = await generator.markMaterialCompleted(mockUser.id, firstMaterialId);

      expect(updated.nextRecommendedMaterial?.id).toBe(secondMaterialId);
    });

    it('should not duplicate completions', async () => {
      const curriculum = await generator.generateCurriculum(mockUser, basicConfig);
      const materialId = curriculum.materials[0]!.id;

      await generator.markMaterialCompleted(mockUser.id, materialId);
      const updated = await generator.markMaterialCompleted(mockUser.id, materialId);

      expect(updated.completedCount).toBe(1);
    });

    it('should throw error for non-existent material', async () => {
      await generator.generateCurriculum(mockUser, basicConfig);

      await expect(
        generator.markMaterialCompleted(mockUser.id, 'non-existent-id')
      ).rejects.toThrow();
    });
  });

  describe('recommendations', () => {
    it('should get recommended next material', async () => {
      await generator.generateCurriculum(mockUser, basicConfig);
      const recommended = generator.getRecommendedNextMaterial(mockUser.id);

      expect(recommended).toBeDefined();
    });

    it('should return undefined when all materials completed', async () => {
      const curriculum = await generator.generateCurriculum(mockUser, basicConfig);

      // Complete all materials
      for (const material of curriculum.materials) {
        await generator.markMaterialCompleted(mockUser.id, material.id);
      }

      const recommended = generator.getRecommendedNextMaterial(mockUser.id);
      expect(recommended).toBeUndefined();
    });

    it('should filter materials by category', async () => {
      await generator.generateCurriculum(mockUser, basicConfig);

      const videos = generator.getMaterialsByCategory(mockUser.id, 'video');
      expect(videos).toBeDefined();
      expect(videos.every((m) => m.category === 'video')).toBe(true);
    });
  });

  describe('metrics', () => {
    it('should track training metrics', async () => {
      await generator.generateCurriculum(mockUser, basicConfig);

      const metrics = generator.getMetrics();

      expect(metrics).toBeDefined();
      expect(metrics.startedCount).toBe(1);
    });

    it('should update metrics on completion', async () => {
      const curriculum = await generator.generateCurriculum(mockUser, basicConfig);

      for (const material of curriculum.materials) {
        await generator.markMaterialCompleted(mockUser.id, material.id);
      }

      const metrics = generator.getMetrics();
      expect(metrics.completedCount).toBeGreaterThan(0);
    });

    it('should calculate completion rate', async () => {
      const curriculum = await generator.generateCurriculum(mockUser, basicConfig);

      for (const material of curriculum.materials) {
        await generator.markMaterialCompleted(mockUser.id, material.id);
      }

      const metrics = generator.getMetrics();
      expect(metrics.completionRate).toBe(100);
    });

    it('should track most accessed materials', async () => {
      await generator.generateCurriculum(mockUser, basicConfig);

      const metrics = generator.getMetrics();
      expect(metrics.mostAccessedMaterials).toBeDefined();
    });

    it('should track difficulty distribution', async () => {
      await generator.generateCurriculum(mockUser, basicConfig);

      const metrics = generator.getMetrics();
      expect(metrics.difficulty_distribution.beginner).toBeGreaterThanOrEqual(0);
      expect(metrics.difficulty_distribution.intermediate).toBeGreaterThanOrEqual(0);
      expect(metrics.difficulty_distribution.advanced).toBeGreaterThanOrEqual(0);
    });
  });

  describe('content library', () => {
    it('should provide access to content library', () => {
      const library = generator.getContentLibrary();

      expect(library.length).toBeGreaterThan(0);
    });

    it('should allow adding custom content', () => {
      const customContent = {
        id: 'custom-content',
        title: 'Custom Tutorial',
        category: 'tutorial' as const,
        contentUrl: '/custom',
        estimatedMinutes: 30,
        difficulty: 'intermediate' as const,
        tags: ['custom'],
      };

      generator.addCustomContent(customContent);

      const library = generator.getContentLibrary();
      expect(library.some((m) => m.id === 'custom-content')).toBe(true);
    });

    it('should prevent duplicate custom content', () => {
      const customContent = {
        id: 'duplicate-id',
        title: 'Custom Tutorial',
        category: 'tutorial' as const,
        contentUrl: '/custom',
        estimatedMinutes: 30,
        difficulty: 'intermediate' as const,
        tags: ['custom'],
      };

      generator.addCustomContent(customContent);

      expect(() => generator.addCustomContent(customContent)).toThrow();
    });
  });

  describe('caching', () => {
    it('should clear cache', async () => {
      await generator.generateCurriculum(mockUser, basicConfig);

      generator.clearCache();

      const curriculum = generator.getCurriculum(mockUser.id);
      expect(curriculum).toBeUndefined();
    });
  });
});
