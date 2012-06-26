package org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils;

import static org.junit.Assert.assertEquals;

import org.siemac.metamac.rest.common.test.utils.MetamacRestAsserts;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public class StatisticalOperationsRestAsserts {

    public static void assertEqualsOperation(Operation expected, Operation actual) {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getKind(), actual.getKind());
        MetamacRestAsserts.assertEqualsLink(expected.getLink(), actual.getLink());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getAcronym(), actual.getAcronym());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getFamilies(), actual.getFamilies());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getSubjectArea(), actual.getSubjectArea());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getSecondarySubjectAreas(), actual.getSecondarySubjectAreas());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getObjective(), actual.getObjective());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDescription(), actual.getDescription());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getInstances(), actual.getInstances());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getSurveyType(), actual.getSurveyType());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getOfficialityType(), actual.getOfficialityType());
        assertEquals(expected.isIndicatorSystem(), actual.isIndicatorSystem());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getProducers(), actual.getProducers());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getRegionalResponsibles(), actual.getRegionalResponsibles());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getRegionalContributors(), actual.getRegionalContributors());
        assertEquals(expected.getInternalInventoryDate(), actual.getInternalInventoryDate());
        assertEquals(expected.isCurrentlyActive(), actual.isCurrentlyActive());
        assertEquals(expected.getStatus(), actual.getStatus());
        assertEquals(expected.getProcStatus(), actual.getProcStatus());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getPublishers(), actual.getPublishers());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getRelPolUsAc(), actual.getRelPolUsAc());
        assertEquals(expected.getRelPolUsAcUrl(), actual.getRelPolUsAcUrl());
        assertEquals(expected.isReleaseCalendar(), actual.isReleaseCalendar());
        assertEquals(expected.getReleaseCalendarAccess(), actual.getReleaseCalendarAccess());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getUpdateFrequencies(), actual.getUpdateFrequencies());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getCurrentInternalInstance(), actual.getCurrentInternalInstance());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getCurrentInstance(), actual.getCurrentInstance());
        assertEquals(expected.getInventoryDate(), actual.getInventoryDate());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getRevPolicy(), actual.getRevPolicy());
        assertEquals(expected.getRevPolicyUrl(), actual.getRevPolicyUrl());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getRevPractice(), actual.getRevPractice());
        assertEquals(expected.getRevPracticeUrl(), actual.getRevPracticeUrl());
        // TODO LEGAL_ACTS, DATA_SHARING, CONFIDENTIALITY_POLICY, CONFIDENTIALITY_DATA_TREATMENT. No están en OperationBase
        MetamacRestAsserts.assertEqualsInternationalString(expected.getComment(), actual.getComment());
        assertEquals(expected.getCommentUrl(), actual.getCommentUrl());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getNotes(), actual.getNotes());
        assertEquals(expected.getNotesUrl(), actual.getNotesUrl());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getParent(), actual.getParent());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getchildren(), actual.getchildren());
    }
    
    public static void assertEqualsFamily(Family expected, Family actual) {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getKind(), actual.getKind());
        MetamacRestAsserts.assertEqualsLink(expected.getLink(), actual.getLink());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getAcronym(), actual.getAcronym());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDescription(), actual.getDescription());
        assertEquals(expected.getInternalInventoryDate(), actual.getInternalInventoryDate());
        assertEquals(expected.getProcStatus(), actual.getProcStatus());
        assertEquals(expected.getInventoryDate(), actual.getInventoryDate());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getParent(), actual.getParent());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getchildren(), actual.getchildren());
    }
    
    public static void assertEqualsInstance(Instance expected, Instance actual) {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getKind(), actual.getKind());
        MetamacRestAsserts.assertEqualsLink(expected.getLink(), actual.getLink());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getAcronym(), actual.getAcronym());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getSurvey(), actual.getSurvey());
        // TODO SUCCESSOR, PREDECESSOR
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDataDescription(), actual.getDataDescription());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getStatisticalPopulation(), actual.getStatisticalPopulation());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getStatisticalUnits(), actual.getStatisticalUnits());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getGeographicGranularity(), actual.getGeographicGranularity());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getGeographicComparability(), actual.getGeographicComparability());
        
        assertEquals(expected.getInternalInventoryDate(), actual.getInternalInventoryDate());
        assertEquals(expected.getProcStatus(), actual.getProcStatus());
        assertEquals(expected.getInventoryDate(), actual.getInventoryDate());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getParent(), actual.getParent());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getchildren(), actual.getchildren());
    }
    
    /**
     * TODO

            
            <xs:element name="temporalGranularity" type="common:RelatedResource" />
            <xs:element name="temporalComparability" type="common:InternationalString" />
            <xs:element name="basePeriod" type="xs:string" />
            <xs:element name="unitMeasure" type="common:RelatedResource" maxOccurs="unbounded"/>
            <xs:element name="statConcDef" type="common:InternationalString" />
            <xs:element name="statConcDefList" type="common:RelatedResource" maxOccurs="unbounded"/>
            <xs:element name="classSystem" type="common:InternationalString" />
            <xs:element name="classSystemList" type="common:RelatedResource" maxOccurs="unbounded"/>
            <xs:element name="instanceType" type="common:RelatedResource" />
            <xs:element name="internalInventoryDate" type="xs:dateTime" />
            <xs:element name="procStatus" type="xs:string" />
            <xs:element name="docMethod" type="common:InternationalString" />
            <xs:element name="surveySource" type="common:RelatedResource" />
            <xs:element name="collMethod" type="common:RelatedResource" />
            <xs:element name="informationSuppliers" type="common:RelatedResource" maxOccurs="unbounded"/>
            <xs:element name="freqColl" type="common:RelatedResource" maxOccurs="unbounded"/>
            <xs:element name="dataValidation" type="common:InternationalString" />
            <xs:element name="dataCompilation" type="common:InternationalString" />
            <xs:element name="adjustment" type="common:InternationalString" />
            <xs:element name="costBurden" type="common:InternationalString" />
            <xs:element name="cost" type="common:RelatedResource" maxOccurs="unbounded"/>
            <xs:element name="inventoryDate" type="xs:dateTime" />
            <xs:element name="qualityDoc" type="common:InternationalString" />
            <xs:element name="qualityAssure" type="common:InternationalString" />
            <xs:element name="qualityAssmnt" type="common:InternationalString" />
            <xs:element name="userNeeds" type="common:InternationalString" />
            <xs:element name="userSat" type="common:InternationalString" />
            <xs:element name="completeness" type="common:InternationalString" />
            <xs:element name="timeliness" type="common:InternationalString" />
            <xs:element name="punctuality" type="common:InternationalString" />
            <xs:element name="accuracyOverall" type="common:InternationalString" />
            <xs:element name="samplingErr" type="common:InternationalString" />
            <xs:element name="nonsamplingErr" type="common:InternationalString" />
            <xs:element name="coherXDom" type="common:InternationalString" />
            <xs:element name="coherInternal" type="common:InternationalString" />
            <xs:element name="comment" type="common:InternationalString" />
            <xs:element name="notes" type="common:InternationalString" />
            <xs:element name="parent" type="common:RelatedResource" />
            <xs:element name="child" type="common:RelatedResource" maxOccurs="unbounded" />
        </xs:sequence>
    </xs:complexType>
     */

}
