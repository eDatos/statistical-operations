package org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils;

import org.joda.time.DateTime;
import org.siemac.metamac.core.common.bt.domain.ExternalItemBt;
import org.siemac.metamac.core.common.ent.domain.InternationalString;
import org.siemac.metamac.core.common.ent.domain.LocalisedString;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.core.common.vo.domain.ExternalItem;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.domain.statistical.operations.enume.domain.StatusEnum;
import org.siemac.metamac.statistical.operations.core.domain.CollMethod;
import org.siemac.metamac.statistical.operations.core.domain.Cost;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.InstanceType;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.SurveySource;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;

public class StatisticalOperationsCoreMocks {

    public static Operation mockOperation1() {

        Operation operation = new Operation();

        operation.setCode("Operation1");
        operation.setTitle(mockInternationalString("es", "Título operation Operation1", "en", "Title operation Operation1"));
        operation.setAcronym(mockInternationalString("es", "Acrónimo 1 en español", "en", "Acronym 1 in English"));
        operation.addFamily(mockFamily("familyCode1"));
        operation.addFamily(mockFamily("familyCode2"));
        operation.setSubjectArea(mockExternalItemBt("subjectArea1", TypeExternalArtefactsEnum.CATEGORY, "http://subjectArea1"));
        operation.getSecondarySubjectAreas().add(mockExternalItem("secundarySubjectArea1", TypeExternalArtefactsEnum.CATEGORY, "http://secundarySubjectArea1"));
        operation.getSecondarySubjectAreas().add(mockExternalItem("secundarySubjectArea2", TypeExternalArtefactsEnum.CATEGORY, "http://secundarySubjectArea2"));
        operation.getSecondarySubjectAreas().add(mockExternalItem("secundarySubjectArea3", TypeExternalArtefactsEnum.CATEGORY, "http://secundarySubjectArea3"));
        operation.setObjective(mockInternationalString("es", "Objetivo 1 en español", "en", "Objective 1 in English"));
        operation.setDescription(mockInternationalString("es", "Descripción 1 en español", "en", "Description 1 in English"));
        operation.addInstance(mockInstance("instanceCode1", ProcStatusEnum.DRAFT));
        operation.addInstance(mockInstance("instanceCode2", ProcStatusEnum.PUBLISH_INTERNALLY));
        operation.addInstance(mockInstance("instanceCode3", ProcStatusEnum.PUBLISH_EXTERNALLY));
        operation.setSurveyType(mockSurveyType("surveyIdentifier"));
        operation.setOfficialityType(mockOfficialityType("officialityTypeIdentifier"));
        operation.setIndicatorSystem(Boolean.TRUE);
        operation.getProducer().add(mockExternalItem("producer1", TypeExternalArtefactsEnum.AGENCY, "http://producer1"));
        operation.getProducer().add(mockExternalItem("producer2", TypeExternalArtefactsEnum.AGENCY, "http://producer2"));
        operation.getRegionalResponsible().add(mockExternalItem("regionalResponsible1", TypeExternalArtefactsEnum.AGENCY, "http://regionalResponsible1"));
        operation.getRegionalResponsible().add(mockExternalItem("regionalResponsible2", TypeExternalArtefactsEnum.AGENCY, "http://regionalResponsible2"));
        operation.getRegionalResponsible().add(mockExternalItem("regionalResponsible3", TypeExternalArtefactsEnum.AGENCY, "http://regionalResponsible3"));
        operation.getRegionalContributor().add(mockExternalItem("regionalContributor1", TypeExternalArtefactsEnum.AGENCY, "http://regionalContributor1"));
        operation.getRegionalContributor().add(mockExternalItem("regionalContributor2", TypeExternalArtefactsEnum.AGENCY, "http://regionalContributor2"));
        operation.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0));
        operation.setCurrentlyActive(Boolean.FALSE);
        operation.setStatus(StatusEnum.DESIGN);
        operation.setProcStatus(ProcStatusEnum.PUBLISH_EXTERNALLY);
        operation.getPublisher().add(mockExternalItem("publisher1", TypeExternalArtefactsEnum.AGENCY, "http://publisher1"));
        operation.getPublisher().add(mockExternalItem("publisher2", TypeExternalArtefactsEnum.AGENCY, "http://publisher2"));
        operation.getPublisher().add(mockExternalItem("publisher3", TypeExternalArtefactsEnum.AGENCY, "http://publisher3"));
        operation.setRelPolUsAc(mockInternationalString("es", "RelPolUsAc 1 en español", "en", "RelPolUsAc 1 in English"));
        operation.setRelPolUsAcUrl("http://relPolUsAc1.url");
        operation.setReleaseCalendar(Boolean.TRUE);
        operation.setReleaseCalendarAccess("http://releaseCalendarAccess1");
        operation.getUpdateFrequency().add(mockExternalItem("updateFrequency1", TypeExternalArtefactsEnum.CODE, "http://updateFrequency1"));
        operation.getUpdateFrequency().add(mockExternalItem("updateFrequency2", TypeExternalArtefactsEnum.CODE, "http://updateFrequency2"));
        operation.getUpdateFrequency().add(mockExternalItem("updateFrequency3", TypeExternalArtefactsEnum.CODE, "http://updateFrequency3"));
        operation.getUpdateFrequency().add(mockExternalItem("updateFrequency4", TypeExternalArtefactsEnum.CODE, "http://updateFrequency4"));
        operation.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0));
        operation.setRevPolicy(mockInternationalString("es", "RevPolicy 1 en español", "en", "RevPolicy 1 in English"));
        operation.setRevPolicyUrl("http://revPolicy1.url");
        operation.setRevPractice(mockInternationalString("es", "RevPractice 1 en español", "en", "RevPractice 1 in English"));
        operation.setRevPracticeUrl("http://revPractice1.url");
        // TODO LEGAL_ACTS, DATA_SHARING, CONFIDENTIALITY_POLICY, CONFIDENTIALITY_DATA_TREATMENT. No están en OperationBase
        operation.setComment(mockInternationalString("es", "Comentarios 1 en español", "en", "Comments 1 in English"));
        operation.setCommentUrl("http://comments1.url");
        operation.setNotes(mockInternationalString("es", "Notas 1 en español", "en", "Notes 1 in English"));
        operation.setNotesUrl("http://notes1.url");

        return operation;
    }
    
    public static Family mockFamily1() {

        Family family = new Family();

        family.setCode("Family1");
        family.setTitle(mockInternationalString("es", "Título 1 en español", "en", "Title 1 in English"));
        family.setAcronym(mockInternationalString("es", "Acrónimo 1 en español", "en", "Acronym 1 in English"));
        family.setDescription(mockInternationalString("es", "Descripción 1 en español", "en", "Description 1 in English"));
        family.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0));
        family.setProcStatus(ProcStatusEnum.PUBLISH_EXTERNALLY);
        family.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0));
        family.addOperation(mockOperation1());
        return family;
    }
    
    public static Instance mockInstance1() {

        Instance instance = new Instance();

        instance.setCode("Instance1");
        instance.setTitle(mockInternationalString("es", "Título 1 en español", "en", "Title 1 in English"));
        instance.setAcronym(mockInternationalString("es", "Acrónimo 1 en español", "en", "Acronym 1 in English"));
        instance.setOperation(mockOperation1());
        // TODO SUCCESSOR, PREDECESSOR
        instance.setDataDescription(mockInternationalString("es", "Descripción de Datos 1 en español", "en", "DataDescription 1 in English"));
        instance.setStatisticalPopulation(mockInternationalString("es", "Carga de Estadísticas 1 en español", "en", "StatisticalPopulation 1 in English"));
        instance.addStatisticalUnit(mockExternalItem("statisticalUnit1", TypeExternalArtefactsEnum.DATASTRUCTURE, "http://statisticalUnit1"));
        instance.addStatisticalUnit(mockExternalItem("statisticalUnit2", TypeExternalArtefactsEnum.DATASTRUCTURE, "http://statisticalUnit2"));
        instance.setGeographicGranularity(mockExternalItemBt("geographicGranularity", TypeExternalArtefactsEnum.CODELIST, "http://geographicGranularity"));
        instance.setGeographicComparability(mockInternationalString("es", "Comparando Geográficos 1 en español", "en", "geographicComparability 1 in English"));
        instance.setTemporalGranularity(mockExternalItemBt("temporalGranularity", TypeExternalArtefactsEnum.CODELIST, "http://temporalGranularity"));
        instance.setTemporalComparability(mockInternationalString("es", "Comparando Temporal 1 en español", "en", "temporalComparability 1 in English"));
        instance.setBasePeriod("2012");
        instance.addUnitMeasure(mockExternalItem("unitMeasure1", TypeExternalArtefactsEnum.CONCEPT, "http://unitMeasure1"));
        instance.setStatConcDef(mockInternationalString("es", "StatConcDef 1 en español", "en", "StatConcDef 1 in English"));
        instance.addStatConcDefList(mockExternalItem("statConcDefList1", TypeExternalArtefactsEnum.CODELIST, "http://statConcDefList1"));
        instance.addStatConcDefList(mockExternalItem("statConcDefList2", TypeExternalArtefactsEnum.CODELIST, "http://statConcDefList2"));
        instance.addStatConcDefList(mockExternalItem("statConcDefList3", TypeExternalArtefactsEnum.CODELIST, "http://statConcDefList3"));
        instance.setClassSystem(mockInternationalString("es", "ClassSystem 1 en español", "en", "ClassSystem 1 in English"));
        instance.addClassSystemList(mockExternalItem("statConcDefList1", TypeExternalArtefactsEnum.CODELIST, "http://statConcDefList1"));
        instance.addClassSystemList(mockExternalItem("statConcDefList2", TypeExternalArtefactsEnum.CODELIST, "http://statConcDefList2"));
        instance.setInstanceType(mockInstanceType("instanceType1"));
        instance.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0));
        instance.setProcStatus(ProcStatusEnum.PUBLISH_EXTERNALLY);
        instance.setDocMethod(mockInternationalString("es", "DocMethod 1 en español", "en", "DocMethod 1 in English"));
        instance.setSurveySource(mockSurveySource("surveySource1"));
        instance.setCollMethod(mockCollMethod("collMethod1"));
        instance.addInformationSupplier(mockExternalItem("informationSupplier1", TypeExternalArtefactsEnum.COMMON_METADATA, "http://informationSupplier1"));
        instance.addFreqColl(mockExternalItem("freqColl1", TypeExternalArtefactsEnum.CATEGORY_SCHEME, "http://freqColl1"));
        instance.addFreqColl(mockExternalItem("freqColl2", TypeExternalArtefactsEnum.CATEGORY_SCHEME, "http://freqColl2"));
        instance.setDataValidation(mockInternationalString("es", "DataValidation 1 en español", "en", "DataValidation 1 in English"));
        instance.setDataCompilation(mockInternationalString("es", "DataCompilation 1 en español", "en", "DataCompilation 1 in English"));
        instance.setAdjustment(mockInternationalString("es", "Adjustment 1 en español", "en", "Adjustment 1 in English"));
        instance.setCostBurden(mockInternationalString("es", "CostBurden 1 en español", "en", "CostBurden 1 in English"));
        instance.addCost(mockCost("cost1"));
        instance.addCost(mockCost("cost2"));
        instance.addCost(mockCost("cost3"));
        instance.addCost(mockCost("cost4"));
        instance.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0));
        instance.setQualityDoc(mockInternationalString("es", "QualityDoc 1 en español", "en", "QualityDoc 1 in English"));
        instance.setQualityAssure(mockInternationalString("es", "QualityAssure 1 en español", "en", "QualityAssure 1 in English"));
        instance.setQualityAssmnt(mockInternationalString("es", "QualityAssmnt 1 en español", "en", "QualityAssmnt 1 in English"));
        instance.setUserNeeds(mockInternationalString("es", "UserNeeds 1 en español", "en", "UserNeeds 1 in English"));
        instance.setUserSat(mockInternationalString("es", "UserSat 1 en español", "en", "UserSat 1 in English"));
        instance.setCompleteness(mockInternationalString("es", "Completeness 1 en español", "en", "Completeness 1 in English"));
        instance.setTimeliness(mockInternationalString("es", "Timeliness 1 en español", "en", "Timeliness 1 in English"));
        instance.setPunctuality(mockInternationalString("es", "Punctuality 1 en español", "en", "Punctuality 1 in English"));
        instance.setAccuracyOverall(mockInternationalString("es", "AccuracyOverall 1 en español", "en", "AccuracyOverall 1 in English"));
        instance.setSamplingErr(mockInternationalString("es", "SamplingErr 1 en español", "en", "SamplingErr 1 in English"));
        instance.setNonsamplingErr(mockInternationalString("es", "NonsamplingErr 1 en español", "en", "NonsamplingErr 1 in English"));
        instance.setCoherXDomain(mockInternationalString("es", "CoherXDom 1 en español", "en", "CoherXDom 1 in English"));
        instance.setCoherInternal(mockInternationalString("es", "CoherInternal 1 en español", "en", "CoherInternal 1 in English"));
        instance.setComment(mockInternationalString("es", "Comentarios 1 en español", "en", "Comments 1 in English"));
        instance.setCommentUrl("http://comments1.url");
        instance.setNotes(mockInternationalString("es", "Notas 1 en español", "en", "Notes 1 in English"));
        instance.setNotesUrl("http://notes1.url");

        return instance;
    }
    
    private static InternationalString mockInternationalString(String locale1, String label1, String locale2, String label2) {

        InternationalString internationalString = new InternationalString();

        LocalisedString internationalStringLocale1 = new LocalisedString();
        internationalStringLocale1.setLocale(locale1);
        internationalStringLocale1.setLabel(label1);
        internationalString.addText(internationalStringLocale1);

        LocalisedString internationalStringLocale2 = new LocalisedString();
        internationalStringLocale2.setLocale(locale2);
        internationalStringLocale2.setLabel(label2);
        internationalString.addText(internationalStringLocale2);

        return internationalString;
    }

    private static Family mockFamily(String code) {
        Family family = new Family();
        family.setCode(code);
        family.setTitle(mockInternationalString("es", "Título family " + code, "en", "Title family " + code));
        family.setDescription(mockInternationalString("es", "Descripción Familia " + code, "en", "Description Family " + code));
        return family;
    }

    private static Instance mockInstance(String code, ProcStatusEnum procStatus) {
        Instance instance = new Instance();
        instance.setCode(code);
        instance.setTitle(mockInternationalString("es", "Título instance " + code, "en", "Title instance " + code));
        instance.setProcStatus(procStatus);
        return instance;
    }

    private static SurveyType mockSurveyType(String code) {
        SurveyType surveyType = new SurveyType();
        surveyType.setDescription(mockInternationalString("es", "Título survey " + code, "en", "Title survey " + code));
        surveyType.setIdentifier(code);
        return surveyType;
    }

    private static OfficialityType mockOfficialityType(String code) {
        OfficialityType officialityType = new OfficialityType();
        officialityType.setDescription(mockInternationalString("es", "Título officialityType " + code, "en", "Title officialityType " + code));
        officialityType.setIdentifier(code);
        return officialityType;
    }
    
    private static InstanceType mockInstanceType(String code) {
        InstanceType instanceType = new InstanceType();
        instanceType.setDescription(mockInternationalString("es", "Título instanceType " + code, "en", "Title instanceType " + code));
        instanceType.setIdentifier(code);
        return instanceType;
    }
    
    private static SurveySource mockSurveySource(String code) {
        SurveySource surveySource = new SurveySource();
        surveySource.setDescription(mockInternationalString("es", "Título surveySource " + code, "en", "Title surveySource " + code));
        surveySource.setIdentifier(code);
        return surveySource;
    }
    
    private static CollMethod mockCollMethod(String code) {
        CollMethod collMethod = new CollMethod();
        collMethod.setDescription(mockInternationalString("es", "Título collMethod " + code, "en", "Title collMethod " + code));
        collMethod.setIdentifier(code);
        return collMethod;
    }
    
    private static Cost mockCost(String code) {
        Cost cost = new Cost();
        cost.setDescription(mockInternationalString("es", "Título cost " + code, "en", "Title cost " + code));
        cost.setIdentifier(code);
        return cost;
    }
    
    private static ExternalItemBt mockExternalItemBt(String code, TypeExternalArtefactsEnum type, String uri) {
        return new ExternalItemBt(uri, code, type);
    }

    private static ExternalItem mockExternalItem(String code, TypeExternalArtefactsEnum type, String uri) {
        return new ExternalItem(new ExternalItemBt(uri, code, type));
    }
}