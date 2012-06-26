package org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils;

import org.joda.time.DateTime;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.rest.common.test.utils.MetamacRestMocks;
import org.siemac.metamac.rest.common.v1_0.domain.RelatedResource;
import org.siemac.metamac.statistical.operations.rest.internal.RestInternalConstants;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public class StatisticalOperationsRestMocks {

    public static Operation mockOperation1(String baseApi) {

        Operation operation = new Operation();

        operation.setId("Operation1");
        operation.setKind(RestInternalConstants.KIND_OPERATION);
        operation.setLink(MetamacRestMocks.mockLink(RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1"));
        operation.setTitle(MetamacRestMocks.mockInternationalString("es", "Título 1 en español", "en", "Title 1 in English"));
        operation.setAcronym(MetamacRestMocks.mockInternationalString("es", "Acrónimo 1 en español", "en", "Acronym 1 in English"));
        operation.getFamilies().add(mockFamily("familyCode1", baseApi));
        operation.getFamilies().add(mockFamily("familyCode2", baseApi));
        operation.setSubjectArea(MetamacRestMocks.mockRelatedResource("subjectArea1", TypeExternalArtefactsEnum.CATEGORY.name(), RestInternalConstants.LINK_SELF, "http://subjectArea1", Boolean.FALSE, null));
        operation.getSecondarySubjectAreas().add(MetamacRestMocks.mockRelatedResource("secundarySubjectArea1", TypeExternalArtefactsEnum.CATEGORY.name(), RestInternalConstants.LINK_SELF, "http://secundarySubjectArea1", Boolean.FALSE, null));
        operation.getSecondarySubjectAreas().add(MetamacRestMocks.mockRelatedResource("secundarySubjectArea2", TypeExternalArtefactsEnum.CATEGORY.name(), RestInternalConstants.LINK_SELF, "http://secundarySubjectArea2", Boolean.FALSE, null));
        operation.getSecondarySubjectAreas().add(MetamacRestMocks.mockRelatedResource("secundarySubjectArea3", TypeExternalArtefactsEnum.CATEGORY.name(), RestInternalConstants.LINK_SELF, "http://secundarySubjectArea3", Boolean.FALSE, null));
        operation.setObjective(MetamacRestMocks.mockInternationalString("es", "Objetivo 1 en español", "en", "Objective 1 in English"));
        operation.setDescription(MetamacRestMocks.mockInternationalString("es", "Descripción 1 en español", "en", "Description 1 in English"));
        operation.getInstances().add(MetamacRestMocks.mockRelatedResource("instanceCode1", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/instances/instanceCode1", Boolean.TRUE, "instance"));
        operation.getInstances().add(MetamacRestMocks.mockRelatedResource("instanceCode2", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/instances/instanceCode2", Boolean.TRUE, "instance"));
        operation.getInstances().add(MetamacRestMocks.mockRelatedResource("instanceCode3", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/instances/instanceCode3", Boolean.TRUE, "instance"));
        operation.setSurveyType(MetamacRestMocks.mockRelatedResource("surveyIdentifier", null, null, null, Boolean.TRUE, "survey"));
        operation.setOfficialityType(MetamacRestMocks.mockRelatedResource("officialityTypeIdentifier", null, null, null, Boolean.TRUE, "officialityType"));
        operation.setIndicatorSystem(Boolean.TRUE);
        operation.getProducers().add(MetamacRestMocks.mockRelatedResource("producer1", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://producer1", Boolean.FALSE, null));
        operation.getProducers().add(MetamacRestMocks.mockRelatedResource("producer2", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://producer2", Boolean.FALSE, null));
        operation.getRegionalResponsibles().add(MetamacRestMocks.mockRelatedResource("regionalResponsible1", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://regionalResponsible1", Boolean.FALSE, null));
        operation.getRegionalResponsibles().add(MetamacRestMocks.mockRelatedResource("regionalResponsible2", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://regionalResponsible2", Boolean.FALSE, null));
        operation.getRegionalResponsibles().add(MetamacRestMocks.mockRelatedResource("regionalResponsible3", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://regionalResponsible3", Boolean.FALSE, null));
        operation.getRegionalContributors().add(MetamacRestMocks.mockRelatedResource("regionalContributor1", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://regionalContributor1", Boolean.FALSE, null));
        operation.getRegionalContributors().add(MetamacRestMocks.mockRelatedResource("regionalContributor2", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://regionalContributor2", Boolean.FALSE, null));
        operation.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        operation.setCurrentlyActive(Boolean.FALSE);
        operation.setStatus("DESIGN");
        operation.setProcStatus("PUBLISH_EXTERNALLY");
        operation.getPublishers().add(MetamacRestMocks.mockRelatedResource("publisher1", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://publisher1", Boolean.FALSE, null));
        operation.getPublishers().add(MetamacRestMocks.mockRelatedResource("publisher2", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://publisher2", Boolean.FALSE, null));
        operation.getPublishers().add(MetamacRestMocks.mockRelatedResource("publisher3", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://publisher3", Boolean.FALSE, null));
        operation.setRelPolUsAc(MetamacRestMocks.mockInternationalString("es", "RelPolUsAc 1 en español", "en", "RelPolUsAc 1 in English"));
        operation.setRelPolUsAcUrl("http://relPolUsAc1.url");
        operation.setReleaseCalendar(Boolean.TRUE);
        operation.setReleaseCalendarAccess("http://releaseCalendarAccess1");
        operation.getUpdateFrequencies().add(MetamacRestMocks.mockRelatedResource("updateFrequency1", TypeExternalArtefactsEnum.CODE.name(), RestInternalConstants.LINK_SELF, "http://updateFrequency1", Boolean.FALSE, null));
        operation.getUpdateFrequencies().add(MetamacRestMocks.mockRelatedResource("updateFrequency2", TypeExternalArtefactsEnum.CODE.name(), RestInternalConstants.LINK_SELF, "http://updateFrequency2", Boolean.FALSE, null));
        operation.getUpdateFrequencies().add(MetamacRestMocks.mockRelatedResource("updateFrequency3", TypeExternalArtefactsEnum.CODE.name(), RestInternalConstants.LINK_SELF, "http://updateFrequency3", Boolean.FALSE, null));
        operation.getUpdateFrequencies().add(MetamacRestMocks.mockRelatedResource("updateFrequency4", TypeExternalArtefactsEnum.CODE.name(), RestInternalConstants.LINK_SELF, "http://updateFrequency4", Boolean.FALSE, null));
        operation.setCurrentInternalInstance(MetamacRestMocks.mockRelatedResource("instanceCode2", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/instances/instanceCode2", Boolean.TRUE, "instance"));
        operation.setCurrentInstance(MetamacRestMocks.mockRelatedResource("instanceCode3", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/instances/instanceCode3", Boolean.TRUE, "instance"));
        operation.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        operation.setRevPolicy(MetamacRestMocks.mockInternationalString("es", "RevPolicy 1 en español", "en", "RevPolicy 1 in English"));
        operation.setRevPolicyUrl("http://revPolicy1.url");
        operation.setRevPractice(MetamacRestMocks.mockInternationalString("es", "RevPractice 1 en español", "en", "RevPractice 1 in English"));
        operation.setRevPracticeUrl("http://revPractice1.url");
        // TODO LEGAL_ACTS, DATA_SHARING, CONFIDENTIALITY_POLICY, CONFIDENTIALITY_DATA_TREATMENT. No están en OperationBase
        operation.setComment(MetamacRestMocks.mockInternationalString("es", "Comentarios 1 en español", "en", "Comments 1 in English"));
        operation.setCommentUrl("http://comments1.url");
        operation.setNotes(MetamacRestMocks.mockInternationalString("es", "Notas 1 en español", "en", "Notes 1 in English"));
        operation.setNotesUrl("http://notes1.url");
        operation.setParent(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_OPERATIONS, RestInternalConstants.LINK_SELF, baseApi + "/operations", Boolean.FALSE, null));
        operation.getchildren().add(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_FAMILIES, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/families", Boolean.FALSE, null));
        operation.getchildren().add(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_INSTANCES, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/instances", Boolean.FALSE, null));
        return operation;
    }
    
    public static Family mockFamily1(String baseApi) {

        Family family = new Family();

        family.setId("Family1");
        family.setKind(RestInternalConstants.KIND_FAMILY);
        family.setLink(MetamacRestMocks.mockLink(RestInternalConstants.LINK_SELF, baseApi + "/families/Family1"));
        family.setTitle(MetamacRestMocks.mockInternationalString("es", "Título 1 en español", "en", "Title 1 in English"));
        family.setAcronym(MetamacRestMocks.mockInternationalString("es", "Acrónimo 1 en español", "en", "Acronym 1 in English"));
        family.setDescription(MetamacRestMocks.mockInternationalString("es", "Descripción 1 en español", "en", "Description 1 in English"));
        family.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        family.setProcStatus("PUBLISH_EXTERNALLY");
        family.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        family.setParent(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_FAMILIES, RestInternalConstants.LINK_SELF, baseApi + "/families", Boolean.FALSE, null));
        family.getchildren().add(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_OPERATIONS, RestInternalConstants.LINK_SELF, baseApi + "/families/Family1/operations", Boolean.FALSE, null));
        return family;
    }

    public static Instance mockInstance1(String baseApi) {

        Instance instance = new Instance();

        instance.setId("Instance1");
        instance.setKind(RestInternalConstants.KIND_INSTANCE);
        instance.setLink(MetamacRestMocks.mockLink(RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/instances/Instance1"));
        instance.setTitle(MetamacRestMocks.mockInternationalString("es", "Título 1 en español", "en", "Title 1 in English"));
        instance.setAcronym(MetamacRestMocks.mockInternationalString("es", "Acrónimo 1 en español", "en", "Acronym 1 in English"));
        instance.setSurvey(MetamacRestMocks.mockRelatedResource("code1", RestInternalConstants.KIND_OPERATION, RestInternalConstants.LINK_SELF, baseApi + "/operations/Code1", Boolean.TRUE, "operation"));
        // TODO SUCCESSOR, PREDECESSOR
        instance.setDataDescription(MetamacRestMocks.mockInternationalString("es", "Descripción de Datos 1 en español", "en", "DataDescription 1 in English"));
        instance.setStatisticalPopulation(MetamacRestMocks.mockInternationalString("es", "Carga de Estadísticas 1 en español", "en", "StatisticalPopulation 1 in English"));
        instance.getStatisticalUnits().add(MetamacRestMocks.mockRelatedResource("statisticalUnit1", TypeExternalArtefactsEnum.DATASTRUCTURE.name(), RestInternalConstants.LINK_SELF, "http://statisticalUnit1", Boolean.FALSE, null));
        instance.getStatisticalUnits().add(MetamacRestMocks.mockRelatedResource("statisticalUnit2", TypeExternalArtefactsEnum.DATASTRUCTURE.name(), RestInternalConstants.LINK_SELF, "http://statisticalUnit2", Boolean.FALSE, null));
        instance.setGeographicGranularity(MetamacRestMocks.mockRelatedResource("geographicGranularity", TypeExternalArtefactsEnum.CODELIST.name(), RestInternalConstants.LINK_SELF, "http://geographicGranularity", Boolean.FALSE, null));
        instance.setGeographicComparability(MetamacRestMocks.mockInternationalString("es", "Comparando Geográficos 1 en español", "en", "geographicComparability 1 in English"));
        instance.setTemporalGranularity(MetamacRestMocks.mockRelatedResource("temporalGranularity", TypeExternalArtefactsEnum.CODELIST.name(), RestInternalConstants.LINK_SELF, "http://temporalGranularity", Boolean.FALSE, null));
        instance.setTemporalComparability(MetamacRestMocks.mockInternationalString("es", "Comparando Temporal 1 en español", "en", "temporalComparability 1 in English"));
        instance.setBasePeriod("2012");
        instance.getUnitMeasures().add(MetamacRestMocks.mockRelatedResource("unitMeasure1", TypeExternalArtefactsEnum.CONCEPT.name(), RestInternalConstants.LINK_SELF, "http://unitMeasure1", Boolean.FALSE, null));
        instance.setStatConcDef(MetamacRestMocks.mockInternationalString("es", "StatConcDef 1 en español", "en", "StatConcDef 1 in English"));
        instance.getStatConcDefLists().add(MetamacRestMocks.mockRelatedResource("statConcDefList1", TypeExternalArtefactsEnum.CODELIST.name(), RestInternalConstants.LINK_SELF, "http://statConcDefList1", Boolean.FALSE, null));
        instance.getStatConcDefLists().add(MetamacRestMocks.mockRelatedResource("statConcDefList2", TypeExternalArtefactsEnum.CODELIST.name(), RestInternalConstants.LINK_SELF, "http://statConcDefList2", Boolean.FALSE, null));
        instance.getStatConcDefLists().add(MetamacRestMocks.mockRelatedResource("statConcDefList3", TypeExternalArtefactsEnum.CODELIST.name(), RestInternalConstants.LINK_SELF, "http://statConcDefList3", Boolean.FALSE, null));
        instance.setClassSystem(MetamacRestMocks.mockInternationalString("es", "ClassSystem 1 en español", "en", "ClassSystem 1 in English"));
        instance.getClassSystemLists().add(MetamacRestMocks.mockRelatedResource("statConcDefList1", TypeExternalArtefactsEnum.CODELIST.name(), RestInternalConstants.LINK_SELF, "http://statConcDefList1", Boolean.FALSE, null));
        instance.getClassSystemLists().add(MetamacRestMocks.mockRelatedResource("statConcDefList2", TypeExternalArtefactsEnum.CODELIST.name(), RestInternalConstants.LINK_SELF, "http://statConcDefList2", Boolean.FALSE, null));
        instance.setInstanceType(MetamacRestMocks.mockRelatedResource("instanceType1", null, null, null, Boolean.TRUE, "instanceType"));
        instance.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        instance.setProcStatus(ProcStatusEnum.PUBLISH_EXTERNALLY.name());
        instance.setDocMethod(MetamacRestMocks.mockInternationalString("es", "DocMethod 1 en español", "en", "DocMethod 1 in English"));
        instance.setSurveySource(MetamacRestMocks.mockRelatedResource("surveySource1", null, null, null, Boolean.TRUE, "surveySource"));
        instance.setCollMethod(MetamacRestMocks.mockRelatedResource("collMethod1", null, null, null, Boolean.TRUE, "collMethod"));
        instance.getInformationSuppliers().add(MetamacRestMocks.mockRelatedResource("informationSupplier1", TypeExternalArtefactsEnum.COMMON_METADATA.name(), RestInternalConstants.LINK_SELF, "http://informationSupplier1", Boolean.FALSE, null));
        instance.getFreqColls().add(MetamacRestMocks.mockRelatedResource("freqColl1", TypeExternalArtefactsEnum.CATEGORY_SCHEME.name(), RestInternalConstants.LINK_SELF, "http://freqColl1", Boolean.FALSE, null));
        instance.getFreqColls().add(MetamacRestMocks.mockRelatedResource("freqColl2", TypeExternalArtefactsEnum.CATEGORY_SCHEME.name(), RestInternalConstants.LINK_SELF, "http://freqColl2", Boolean.FALSE, null));
        instance.setDataValidation(MetamacRestMocks.mockInternationalString("es", "DataValidation 1 en español", "en", "DataValidation 1 in English"));
        instance.setDataCompilation(MetamacRestMocks.mockInternationalString("es", "DataCompilation 1 en español", "en", "DataCompilation 1 in English"));
        instance.setAdjustment(MetamacRestMocks.mockInternationalString("es", "Adjustment 1 en español", "en", "Adjustment 1 in English"));
        instance.setCostBurden(MetamacRestMocks.mockInternationalString("es", "CostBurden 1 en español", "en", "CostBurden 1 in English"));
        instance.getCosts().add(MetamacRestMocks.mockRelatedResource("cost1", null, null, null, Boolean.TRUE, "cost"));
        instance.getCosts().add(MetamacRestMocks.mockRelatedResource("cost2", null, null, null, Boolean.TRUE, "cost"));
        instance.getCosts().add(MetamacRestMocks.mockRelatedResource("cost3", null, null, null, Boolean.TRUE, "cost"));
        instance.getCosts().add(MetamacRestMocks.mockRelatedResource("cost4", null, null, null, Boolean.TRUE, "cost"));
        instance.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        instance.setQualityDoc(MetamacRestMocks.mockInternationalString("es", "QualityDoc 1 en español", "en", "QualityDoc 1 in English"));
        instance.setQualityAssure(MetamacRestMocks.mockInternationalString("es", "QualityAssure 1 en español", "en", "QualityAssure 1 in English"));
        instance.setQualityAssmnt(MetamacRestMocks.mockInternationalString("es", "QualityAssmnt 1 en español", "en", "QualityAssmnt 1 in English"));
        instance.setUserNeeds(MetamacRestMocks.mockInternationalString("es", "UserNeeds 1 en español", "en", "UserNeeds 1 in English"));
        instance.setUserSat(MetamacRestMocks.mockInternationalString("es", "UserSat 1 en español", "en", "UserSat 1 in English"));
        instance.setCompleteness(MetamacRestMocks.mockInternationalString("es", "Completeness 1 en español", "en", "Completeness 1 in English"));
        instance.setTimeliness(MetamacRestMocks.mockInternationalString("es", "Timeliness 1 en español", "en", "Timeliness 1 in English"));
        instance.setPunctuality(MetamacRestMocks.mockInternationalString("es", "Punctuality 1 en español", "en", "Punctuality 1 in English"));
        instance.setAccuracyOverall(MetamacRestMocks.mockInternationalString("es", "AccuracyOverall 1 en español", "en", "AccuracyOverall 1 in English"));
        instance.setSamplingErr(MetamacRestMocks.mockInternationalString("es", "SamplingErr 1 en español", "en", "SamplingErr 1 in English"));
        instance.setNonsamplingErr(MetamacRestMocks.mockInternationalString("es", "NonsamplingErr 1 en español", "en", "NonsamplingErr 1 in English"));
        instance.setCoherXDom(MetamacRestMocks.mockInternationalString("es", "CoherXDom 1 en español", "en", "CoherXDom 1 in English"));
        instance.setCoherInternal(MetamacRestMocks.mockInternationalString("es", "CoherInternal 1 en español", "en", "CoherInternal 1 in English"));
        instance.setComment(MetamacRestMocks.mockInternationalString("es", "Comentarios 1 en español", "en", "Comments 1 in English"));
        instance.setNotes(MetamacRestMocks.mockInternationalString("es", "Notas 1 en español", "en", "Notes 1 in English"));
        instance.setParent(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_OPERATION, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1", Boolean.FALSE, null));
        instance.getchildren(); // no children
        return instance;
    }

    private static RelatedResource mockFamily(String id, String baseApi) {
        return MetamacRestMocks.mockRelatedResource(id, RestInternalConstants.KIND_FAMILY, RestInternalConstants.LINK_SELF, baseApi + "/families/" + id, Boolean.TRUE, "family");
    }
}
