package org.siemac.metamac.statistical.operations.rest.internal;

import org.joda.time.DateTime;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.rest.common.test.utils.MetamacRestMocks;
import org.siemac.metamac.rest.common.v1_0.domain.RelatedResource;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public class StatisticalOperationsRestMocks {

    public static Operation mockOperation1(String baseApi) {

        Operation operation = new Operation();

        operation.setId("Code1");
        operation.setKind(RestInternalConstants.KIND_OPERATION);
        operation.setLink(MetamacRestMocks.mockLink(RestInternalConstants.LINK_SELF, baseApi + "/operations/Code1"));
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
        operation.getInstances().add(MetamacRestMocks.mockRelatedResource("instanceCode1", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Code1/instances/instanceCode1", Boolean.TRUE, "instance"));
        operation.getInstances().add(MetamacRestMocks.mockRelatedResource("instanceCode2", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Code1/instances/instanceCode2", Boolean.TRUE, "instance"));
        operation.getInstances().add(MetamacRestMocks.mockRelatedResource("instanceCode3", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Code1/instances/instanceCode3", Boolean.TRUE, "instance"));
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
        operation.setCurrentInternalInstance(MetamacRestMocks.mockRelatedResource("instanceCode2", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Code1/instances/instanceCode2", Boolean.TRUE, "instance"));
        operation.setCurrentInstance(MetamacRestMocks.mockRelatedResource("instanceCode3", RestInternalConstants.KIND_INSTANCE, RestInternalConstants.LINK_SELF, baseApi + "/operations/Code1/instances/instanceCode3", Boolean.TRUE, "instance"));
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
        operation.getchildren().add(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_FAMILIES, RestInternalConstants.LINK_SELF, baseApi + "/operations/Code1/families", Boolean.FALSE, null));
        operation.getchildren().add(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_INSTANCES, RestInternalConstants.LINK_SELF, baseApi + "/operations/Code1/instances", Boolean.FALSE, null));
        return operation;
    }
    
    public static Family mockFamily1(String baseApi) {

        Family family = new Family();

        family.setId("Code1");
        family.setKind(RestInternalConstants.KIND_FAMILY);
        family.setLink(MetamacRestMocks.mockLink(RestInternalConstants.LINK_SELF, baseApi + "/families/Code1"));
        family.setTitle(MetamacRestMocks.mockInternationalString("es", "Título 1 en español", "en", "Title 1 in English"));
        family.setAcronym(MetamacRestMocks.mockInternationalString("es", "Acrónimo 1 en español", "en", "Acronym 1 in English"));
        family.setDescription(MetamacRestMocks.mockInternationalString("es", "Descripción 1 en español", "en", "Description 1 in English"));
        family.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        family.setProcStatus("PUBLISH_EXTERNALLY");
        family.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        family.setParent(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_FAMILIES, RestInternalConstants.LINK_SELF, baseApi + "/families", Boolean.FALSE, null));
        family.getchildren().add(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_OPERATIONS, RestInternalConstants.LINK_SELF, baseApi + "/families/Code1/operations", Boolean.FALSE, null));
        return family;
    }

/**
 * TODO
 * CODE
URI
TITLE
ACRONYM
SURVEY_CODE
SURVEY_TITLE
SUCCESSOR
PREDECESSOR
DATA_DESCRIPTION
STATISTICAL_POPULATION
STATISTICAL_UNIT
GEOGRAPHIC_GRANULARITY
GEOGRAPHIC_COMPARABILITY
TEMPORAL_GRANULARITY
TEMPORAL_COMPARABILITY
BASE_PERIOD
UNIT_MEASURE
STAT_CONC_DEF
STAT_CONC_DEF_LIST
CLASS_SYSTEM
CLASS_SYSTEM_LIST
INSTANCE_TYPES
INTERNAL_INVENTORY_DATE
PROC_STATUS
DOC_METHOD
SURVEY_SOURCE
COLL_METHOD
INFORMATION_SUPPLIERS
FREQ_COLL
DATA_VALIDATION
DATA_COMPILATION
ADJUSTMENT
COST_BURDEN
COST
INVENTORY_DATE
QUALITY_DOC
QUALITY_ASSURE
QUALITY_ASSMNT
USER_NEEDS
USER_SAT
COMPLETENESS
TIMELINESS
PUNCTUALITY
ACCURACY_OVERALL
SAMPLING_ERR
NONSAMPLING_ERR
COHER_X_DOM
COHER_INTERNAL
COMMENT
NOTES

 * @param id
 * @param baseApi
 * @return
 */
    private static RelatedResource mockFamily(String id, String baseApi) {
        return MetamacRestMocks.mockRelatedResource(id, RestInternalConstants.KIND_FAMILY, RestInternalConstants.LINK_SELF, baseApi + "/families/" + id, Boolean.TRUE, "family");
    }
}
