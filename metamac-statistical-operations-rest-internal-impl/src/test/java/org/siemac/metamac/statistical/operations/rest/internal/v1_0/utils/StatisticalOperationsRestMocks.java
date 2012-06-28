package org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils;

import java.math.BigInteger;

import org.joda.time.DateTime;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.domain.statistical.operations.enume.domain.StatusEnum;
import org.siemac.metamac.rest.common.test.utils.MetamacRestMocks;
import org.siemac.metamac.rest.common.v1_0.domain.RelatedResource;
import org.siemac.metamac.statistical.operations.rest.internal.RestInternalConstants;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.FamiliesNoPagedResult;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public class StatisticalOperationsRestMocks {

    public static Operation mockOperation1(String baseApi) {

        Operation operation = new Operation();
        operation.setId("Operation1");
        operation.setKind(RestInternalConstants.KIND_OPERATION);
        operation.setLink(MetamacRestMocks.mockLink(RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1"));
        operation.setTitle(MetamacRestMocks.mockInternationalString("es", "Título operation Operation1", "en", "Title operation Operation1"));
        operation.setAcronym(MetamacRestMocks.mockInternationalString("es", "Acrónimo 1 en español", "en", "Acronym 1 in English"));
        operation.getFamilies().add(mockFamily("familyCode1", baseApi));
        operation.getFamilies().add(mockFamily("familyCode22", baseApi));
        operation.setSubjectArea(MetamacRestMocks.mockRelatedResource("subjectArea1", TypeExternalArtefactsEnum.CATEGORY.name(), RestInternalConstants.LINK_SELF, "http://subjectArea1", Boolean.FALSE, null));
        operation.getSecondarySubjectAreas().add(MetamacRestMocks.mockRelatedResource("secundarySubjectArea1", TypeExternalArtefactsEnum.CATEGORY.name(), RestInternalConstants.LINK_SELF, "http://secundarySubjectArea1", Boolean.FALSE, null));
        operation.getSecondarySubjectAreas().add(MetamacRestMocks.mockRelatedResource("secundarySubjectArea22", TypeExternalArtefactsEnum.CATEGORY.name(), RestInternalConstants.LINK_SELF, "http://secundarySubjectArea22", Boolean.FALSE, null));
        operation.getSecondarySubjectAreas().add(MetamacRestMocks.mockRelatedResource("secundarySubjectArea333", TypeExternalArtefactsEnum.CATEGORY.name(), RestInternalConstants.LINK_SELF, "http://secundarySubjectArea333", Boolean.FALSE, null));
        operation.setObjective(MetamacRestMocks.mockInternationalString("es", "Objetivo 1 en español", "en", "Objective 1 in English"));
        operation.setDescription(MetamacRestMocks.mockInternationalString("es", "Descripción 1 en español", "en", "Description 1 in English"));
        operation.getInstances().add(MetamacRestMocks.mockRelatedResource("instance1", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/instances/instance1", Boolean.TRUE, "instance"));
        operation.getInstances().add(MetamacRestMocks.mockRelatedResource("instanceCode22", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/instances/instanceCode22", Boolean.TRUE, "instance"));
        operation.getInstances().add(MetamacRestMocks.mockRelatedResource("instanceCode333", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/instances/instanceCode333", Boolean.TRUE, "instance"));
        operation.getInstances().add(MetamacRestMocks.mockRelatedResource("instanceCode333", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/instances/instanceCode333", Boolean.TRUE, "instance"));
        operation.setSurveyType(MetamacRestMocks.mockRelatedResource("surveyIdentifier", null, null, null, Boolean.TRUE, "survey"));
        operation.setOfficialityType(MetamacRestMocks.mockRelatedResource("officialityTypeIdentifier", null, null, null, Boolean.TRUE, "officialityType"));
        operation.setIndicatorSystem(Boolean.TRUE);
        operation.getProducers().add(MetamacRestMocks.mockRelatedResource("producer1", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://producer1", Boolean.FALSE, null));
        operation.getProducers().add(MetamacRestMocks.mockRelatedResource("producer22", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://producer22", Boolean.FALSE, null));
        operation.getRegionalResponsibles().add(MetamacRestMocks.mockRelatedResource("regionalResponsible1", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://regionalResponsible1", Boolean.FALSE, null));
        operation.getRegionalResponsibles().add(MetamacRestMocks.mockRelatedResource("regionalResponsible22", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://regionalResponsible22", Boolean.FALSE, null));
        operation.getRegionalResponsibles().add(MetamacRestMocks.mockRelatedResource("regionalResponsible333", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://regionalResponsible333", Boolean.FALSE, null));
        operation.getRegionalContributors().add(MetamacRestMocks.mockRelatedResource("regionalContributor1", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://regionalContributor1", Boolean.FALSE, null));
        operation.getRegionalContributors().add(MetamacRestMocks.mockRelatedResource("regionalContributor22", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://regionalContributor22", Boolean.FALSE, null));
        operation.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        operation.setCurrentlyActive(Boolean.FALSE);
        operation.setStatus(StatusEnum.DESIGN.name());
        operation.setProcStatus(ProcStatusEnum.PUBLISH_INTERNALLY.name());
        operation.getPublishers().add(MetamacRestMocks.mockRelatedResource("publisher1", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://publisher1", Boolean.FALSE, null));
        operation.getPublishers().add(MetamacRestMocks.mockRelatedResource("publisher22", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://publisher22", Boolean.FALSE, null));
        operation.getPublishers().add(MetamacRestMocks.mockRelatedResource("publisher333", TypeExternalArtefactsEnum.AGENCY.name(), RestInternalConstants.LINK_SELF, "http://publisher333", Boolean.FALSE, null));
        operation.setRelPolUsAc(MetamacRestMocks.mockInternationalString("es", "RelPolUsAc 1 en español", "en", "RelPolUsAc 1 in English"));
        operation.setReleaseCalendar(Boolean.TRUE);
        operation.setReleaseCalendarAccess("http://releaseCalendarAccess1");
        operation.getUpdateFrequencies().add(MetamacRestMocks.mockRelatedResource("updateFrequency1", TypeExternalArtefactsEnum.CODE.name(), RestInternalConstants.LINK_SELF, "http://updateFrequency1", Boolean.FALSE, null));
        operation.getUpdateFrequencies().add(MetamacRestMocks.mockRelatedResource("updateFrequency22", TypeExternalArtefactsEnum.CODE.name(), RestInternalConstants.LINK_SELF, "http://updateFrequency22", Boolean.FALSE, null));
        operation.getUpdateFrequencies().add(MetamacRestMocks.mockRelatedResource("updateFrequency333", TypeExternalArtefactsEnum.CODE.name(), RestInternalConstants.LINK_SELF, "http://updateFrequency333", Boolean.FALSE, null));
        operation.getUpdateFrequencies().add(MetamacRestMocks.mockRelatedResource("updateFrequency4444", TypeExternalArtefactsEnum.CODE.name(), RestInternalConstants.LINK_SELF, "http://updateFrequency4444", Boolean.FALSE, null));
        operation.setCurrentInternalInstance(MetamacRestMocks.mockRelatedResource("instanceCode22", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/instances/instanceCode22", Boolean.TRUE, "instance"));
        operation.setCurrentInstance(MetamacRestMocks.mockRelatedResource("instanceCode333", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/instances/instanceCode333", Boolean.TRUE, "instance"));
        operation.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        operation.setRevPolicy(MetamacRestMocks.mockInternationalString("es", "RevPolicy 1 en español", "en", "RevPolicy 1 in English"));
        operation.setRevPractice(MetamacRestMocks.mockInternationalString("es", "RevPractice 1 en español", "en", "RevPractice 1 in English"));
        // TODO CONTACTS, LEGAL_ACTS, DATA_SHARING, CONFIDENTIALITY_POLICY, CONFIDENTIALITY_DATA_TREATMENT. No están en OperationBase
        operation.setComment(MetamacRestMocks.mockInternationalString("es", "Comentarios 1 en español", "en", "Comments 1 in English"));
        operation.setNotes(MetamacRestMocks.mockInternationalString("es", "Notas 1 en español", "en", "Notes 1 in English"));
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
        family.setProcStatus(ProcStatusEnum.PUBLISH_INTERNALLY.name());
        family.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        family.setParent(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_FAMILIES, RestInternalConstants.LINK_SELF, baseApi + "/families", Boolean.FALSE, null));
        family.getchildren().add(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_OPERATIONS, RestInternalConstants.LINK_SELF, baseApi + "/families/Family1/operations", Boolean.FALSE, null));
        return family;
    }
    
    public static Family mockFamily2(String baseApi) {

        Family family = new Family();
        family.setId("Family2");
        family.setKind(RestInternalConstants.KIND_FAMILY);
        family.setLink(MetamacRestMocks.mockLink(RestInternalConstants.LINK_SELF, baseApi + "/families/Family2"));
        family.setTitle(MetamacRestMocks.mockInternationalString("es", "Título 2 en español", "en", "Title 2 in English"));
        family.setAcronym(MetamacRestMocks.mockInternationalString("es", "Acrónimo 2 en español", "en", "Acronym 2 in English"));
        family.setDescription(MetamacRestMocks.mockInternationalString("es", "Descripción 2 en español", "en", "Description 2 in English"));
        family.setInternalInventoryDate(new DateTime(2011, 1, 3, 6, 16, 17, 0).toDate());
        family.setProcStatus(ProcStatusEnum.PUBLISH_EXTERNALLY.name());
        family.setInventoryDate(new DateTime(2013, 5, 14, 23, 15, 14, 0).toDate());
        family.setParent(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_FAMILIES, RestInternalConstants.LINK_SELF, baseApi + "/families", Boolean.FALSE, null));
        family.getchildren().add(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_OPERATIONS, RestInternalConstants.LINK_SELF, baseApi + "/families/Family2/operations", Boolean.FALSE, null));
        return family;
    }

    public static Instance mockInstance1(String baseApi) {

        Instance instance = new Instance();
        instance.setId("Instance1");
        instance.setKind(RestInternalConstants.KIND_INSTANCE);
        instance.setLink(MetamacRestMocks.mockLink(RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/instances/Instance1"));
        instance.setTitle(MetamacRestMocks.mockInternationalString("es", "Título 1 en español", "en", "Title 1 in English"));
        instance.setAcronym(MetamacRestMocks.mockInternationalString("es", "Acrónimo 1 en español", "en", "Acronym 1 in English"));
        instance.setSurvey(MetamacRestMocks.mockRelatedResource("Operation1", RestInternalConstants.KIND_OPERATION, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1", Boolean.TRUE, "operation"));
        instance.setPredecessor(MetamacRestMocks.mockRelatedResource("instanceCode333", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/instances/instanceCode333", Boolean.TRUE, "instance"));
        instance.setSuccessor(MetamacRestMocks.mockRelatedResource("instanceCode22", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Operation1/instances/instanceCode22", Boolean.TRUE, "instance"));
        instance.setDataDescription(MetamacRestMocks.mockInternationalString("es", "Descripción de Datos 1 en español", "en", "DataDescription 1 in English"));
        instance.setStatisticalPopulation(MetamacRestMocks.mockInternationalString("es", "Carga de Estadísticas 1 en español", "en", "StatisticalPopulation 1 in English"));
        instance.getStatisticalUnits().add(MetamacRestMocks.mockRelatedResource("statisticalUnit1", TypeExternalArtefactsEnum.DATASTRUCTURE.name(), RestInternalConstants.LINK_SELF, "http://statisticalUnit1", Boolean.FALSE, null));
        instance.getStatisticalUnits().add(MetamacRestMocks.mockRelatedResource("statisticalUnit22", TypeExternalArtefactsEnum.DATASTRUCTURE.name(), RestInternalConstants.LINK_SELF, "http://statisticalUnit22", Boolean.FALSE, null));
        instance.setGeographicGranularity(MetamacRestMocks.mockRelatedResource("geographicGranularity", TypeExternalArtefactsEnum.CODELIST.name(), RestInternalConstants.LINK_SELF, "http://geographicGranularity", Boolean.FALSE, null));
        instance.setGeographicComparability(MetamacRestMocks.mockInternationalString("es", "Comparando Geográficos 1 en español", "en", "geographicComparability 1 in English"));
        instance.setTemporalGranularity(MetamacRestMocks.mockRelatedResource("temporalGranularity", TypeExternalArtefactsEnum.CODELIST.name(), RestInternalConstants.LINK_SELF, "http://temporalGranularity", Boolean.FALSE, null));
        instance.setTemporalComparability(MetamacRestMocks.mockInternationalString("es", "Comparando Temporal 1 en español", "en", "temporalComparability 1 in English"));
        instance.setBasePeriod("2012");
        instance.getUnitMeasures().add(MetamacRestMocks.mockRelatedResource("unitMeasure1", TypeExternalArtefactsEnum.CONCEPT.name(), RestInternalConstants.LINK_SELF, "http://unitMeasure1", Boolean.FALSE, null));
        instance.setStatConcDef(MetamacRestMocks.mockInternationalString("es", "StatConcDef 1 en español", "en", "StatConcDef 1 in English"));
        instance.getStatConcDefLists().add(MetamacRestMocks.mockRelatedResource("statConcDefList1", TypeExternalArtefactsEnum.CODELIST.name(), RestInternalConstants.LINK_SELF, "http://statConcDefList1", Boolean.FALSE, null));
        instance.getStatConcDefLists().add(MetamacRestMocks.mockRelatedResource("statConcDefList22", TypeExternalArtefactsEnum.CODELIST.name(), RestInternalConstants.LINK_SELF, "http://statConcDefList22", Boolean.FALSE, null));
        instance.getStatConcDefLists().add(MetamacRestMocks.mockRelatedResource("statConcDefList333", TypeExternalArtefactsEnum.CODELIST.name(), RestInternalConstants.LINK_SELF, "http://statConcDefList333", Boolean.FALSE, null));
        instance.setClassSystem(MetamacRestMocks.mockInternationalString("es", "ClassSystem 1 en español", "en", "ClassSystem 1 in English"));
        instance.getClassSystemLists().add(MetamacRestMocks.mockRelatedResource("statConcDefList1", TypeExternalArtefactsEnum.CODELIST.name(), RestInternalConstants.LINK_SELF, "http://statConcDefList1", Boolean.FALSE, null));
        instance.getClassSystemLists().add(MetamacRestMocks.mockRelatedResource("statConcDefList22", TypeExternalArtefactsEnum.CODELIST.name(), RestInternalConstants.LINK_SELF, "http://statConcDefList22", Boolean.FALSE, null));
        instance.setInstanceType(MetamacRestMocks.mockRelatedResource("instanceType1", null, null, null, Boolean.TRUE, "instanceType"));
        instance.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        instance.setProcStatus(ProcStatusEnum.PUBLISH_INTERNALLY.name());
        instance.setDocMethod(MetamacRestMocks.mockInternationalString("es", "DocMethod 1 en español", "en", "DocMethod 1 in English"));
        instance.setSurveySource(MetamacRestMocks.mockRelatedResource("surveySource1", null, null, null, Boolean.TRUE, "surveySource"));
        instance.setCollMethod(MetamacRestMocks.mockRelatedResource("collMethod1", null, null, null, Boolean.TRUE, "collMethod"));
        instance.getInformationSuppliers().add(MetamacRestMocks.mockRelatedResource("informationSupplier1", TypeExternalArtefactsEnum.COMMON_METADATA.name(), RestInternalConstants.LINK_SELF, "http://informationSupplier1", Boolean.FALSE, null));
        instance.getFreqColls().add(MetamacRestMocks.mockRelatedResource("freqColl1", TypeExternalArtefactsEnum.CATEGORY_SCHEME.name(), RestInternalConstants.LINK_SELF, "http://freqColl1", Boolean.FALSE, null));
        instance.getFreqColls().add(MetamacRestMocks.mockRelatedResource("freqColl22", TypeExternalArtefactsEnum.CATEGORY_SCHEME.name(), RestInternalConstants.LINK_SELF, "http://freqColl22", Boolean.FALSE, null));
        instance.setDataValidation(MetamacRestMocks.mockInternationalString("es", "DataValidation 1 en español", "en", "DataValidation 1 in English"));
        instance.setDataCompilation(MetamacRestMocks.mockInternationalString("es", "DataCompilation 1 en español", "en", "DataCompilation 1 in English"));
        instance.setAdjustment(MetamacRestMocks.mockInternationalString("es", "Adjustment 1 en español", "en", "Adjustment 1 in English"));
        instance.setCostBurden(MetamacRestMocks.mockInternationalString("es", "CostBurden 1 en español", "en", "CostBurden 1 in English"));
        instance.getCosts().add(MetamacRestMocks.mockRelatedResource("cost1", null, null, null, Boolean.TRUE, "cost"));
        instance.getCosts().add(MetamacRestMocks.mockRelatedResource("cost22", null, null, null, Boolean.TRUE, "cost"));
        instance.getCosts().add(MetamacRestMocks.mockRelatedResource("cost333", null, null, null, Boolean.TRUE, "cost"));
        instance.getCosts().add(MetamacRestMocks.mockRelatedResource("cost4444", null, null, null, Boolean.TRUE, "cost"));
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
    
    public static FamiliesNoPagedResult mockFamiliesNoPagedResultByOperation1(String baseApi) {
        FamiliesNoPagedResult familiesResult = new FamiliesNoPagedResult();
        familiesResult.setKind(RestInternalConstants.KIND_FAMILIES);
        familiesResult.setTotal(BigInteger.valueOf(2));
        familiesResult.getItems().add(mockFamily1(baseApi));
        familiesResult.getItems().add(mockFamily2(baseApi));
        return familiesResult;
    }

    private static RelatedResource mockFamily(String id, String baseApi) {
        return MetamacRestMocks.mockRelatedResource(id, RestInternalConstants.KIND_FAMILY, RestInternalConstants.LINK_SELF, baseApi + "/families/" + id, Boolean.TRUE, "family");
    }


}
