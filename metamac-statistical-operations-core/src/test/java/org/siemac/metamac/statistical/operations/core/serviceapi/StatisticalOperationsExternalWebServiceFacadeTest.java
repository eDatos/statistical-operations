package org.siemac.metamac.statistical.operations.core.serviceapi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.util.Date;
import java.util.UUID;

import org.apache.commons.lang.RandomStringUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.core.common.dto.LocalisedStringDto;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.domain.statistical.operations.enume.domain.StatusEnum;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacCriteria;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacCriteriaConjunctionRestriction;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacCriteriaDisjunctionRestriction;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacCriteriaPropertyRestriction;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacCriteriaRestrictionList;
import org.siemac.metamac.schema.common.v1_0.domain.OperationType;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.external.ws.v1_0.MetamacExceptionFault;
import org.siemac.metamac.statistical.operations.external.ws.v1_0.MetamacStatisticalOperationsExternalInterfaceV10;
import org.siemac.metamac.statistical.operations.external.ws.v1_0.domain.FindOperationsResult;
import org.siemac.metamac.statistical.operations.external.ws.v1_0.domain.OperationBase;
import org.siemac.metamac.statistical.operations.external.ws.v1_0.domain.OperationCriteriaPropertyRestriction;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * Spring based transactional test with DbUnit support.
 */

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:spring/statistical-operations/applicationContext-test.xml"})
public class StatisticalOperationsExternalWebServiceFacadeTest extends StatisticalOperationsBaseTest {

    @Autowired
    protected StatisticalOperationsServiceFacade               statisticalOperationsServiceFacade;

    @Autowired
    protected MetamacStatisticalOperationsExternalInterfaceV10 metamacStatisticalOperationsExternalInterfaceV10;

    @Test
    public void testRetrieveOperation() throws Exception {

        // Create and publish
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        // Web service: retrieve operartion
        OperationBase operationBase = metamacStatisticalOperationsExternalInterfaceV10.retrieveOperation(operationDto.getCode());

        assertNotNull(operationBase);
        assertEquals(operationDto.getCode(), operationBase.getCode());
    }

    @Test
    public void testRetrieveOperationErrorNotPublishedExternally() throws Exception {

        // Create and publish
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        // Checks exists
        OperationDto operationDtoFacade = statisticalOperationsServiceFacade.findOperationByCode(getServiceContextAdministrador(), operationDto.getCode());
        assertNotNull(operationDtoFacade);
        assertEquals(ProcStatusEnum.PUBLISH_INTERNALLY, operationDtoFacade.getProcStatus());
        
        // Webservice external doesnt find it
        try {
            metamacStatisticalOperationsExternalInterfaceV10.retrieveOperation(operationDto.getCode());
            fail("operation not exists");
        } catch (MetamacExceptionFault e) {
            assertEquals(BigInteger.ONE, e.getFaultInfo().getExceptionItems().getTotal());
            assertEquals(ServiceExceptionType.OPERATION_NOT_FOUND.toString(), e.getFaultInfo().getExceptionItems().getMetamacExceptionItem().get(0).getCode());
            assertEquals(1, e.getFaultInfo().getExceptionItems().getMetamacExceptionItem().get(0).getMessageParameters().size());
            assertEquals(operationDto.getCode(), e.getFaultInfo().getExceptionItems().getMetamacExceptionItem().get(0).getMessageParameters().get(0));
        }
    }

    @Test
    public void testRetrieveOperationErrorNotExists() throws Exception {
        String notExists = UUID.randomUUID().toString();
        try {
            metamacStatisticalOperationsExternalInterfaceV10.retrieveOperation(notExists);
            fail("operation not exists");
        } catch (MetamacExceptionFault e) {
            assertEquals(BigInteger.ONE, e.getFaultInfo().getExceptionItems().getTotal());
            assertEquals(ServiceExceptionType.OPERATION_NOT_FOUND.toString(), e.getFaultInfo().getExceptionItems().getMetamacExceptionItem().get(0).getCode());
            assertEquals(1, e.getFaultInfo().getExceptionItems().getMetamacExceptionItem().get(0).getMessageParameters().size());
            assertEquals(notExists, e.getFaultInfo().getExceptionItems().getMetamacExceptionItem().get(0).getMessageParameters().get(0));
        }
    }

    @Test
    public void testFindOperations() throws Exception {

        // Create and publish internally operation 1
        OperationDto operationDto1 = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto1.getId());
        // Create and publish externally operation 2
        OperationDto operationDto2 = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto2.getId());
        statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextAdministrador(), operationDto2.getId());
        // Create and publish externally operation 3
        OperationDto operationDto3 = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto3.getId());
        statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextAdministrador(), operationDto3.getId());

        {
            // Find operations without proc status property restriction
            MetamacCriteria metamacCriteria = new MetamacCriteria();
            metamacCriteria.setRestriction(new MetamacCriteriaConjunctionRestriction());
            ((MetamacCriteriaConjunctionRestriction) metamacCriteria.getRestriction()).setRestrictions(new MetamacCriteriaRestrictionList());

            // Is indicators system
            MetamacCriteriaPropertyRestriction isIndicatorsSystemPropertyRestriction = new MetamacCriteriaPropertyRestriction();
            isIndicatorsSystemPropertyRestriction.setPropertyName(OperationCriteriaPropertyRestriction.IS_INDICATORS_SYSTEM.value());
            isIndicatorsSystemPropertyRestriction.setOperation(OperationType.EQ);
            isIndicatorsSystemPropertyRestriction.setBooleanValue(Boolean.TRUE);
            ((MetamacCriteriaConjunctionRestriction) metamacCriteria.getRestriction()).getRestrictions().getRestriction().add(isIndicatorsSystemPropertyRestriction);

            // Find
            FindOperationsResult findOperationsResult = metamacStatisticalOperationsExternalInterfaceV10.findOperations(metamacCriteria);

            // Validate
            assertNotNull(findOperationsResult.getOperations());
            boolean operation1Exists = false;
            boolean operation2Exists = false;
            boolean operation3Exists = false;
            for (OperationBase operationBase : findOperationsResult.getOperations().getOperation()) {
                if (operationBase.getCode().equals(operationDto1.getCode())) {
                    operation1Exists = true;
                } else if (operationBase.getCode().equals(operationDto2.getCode())) {
                    operation2Exists = true;
                } else if (operationBase.getCode().equals(operationDto3.getCode())) {
                    operation3Exists = true;
                }
            }
            assertFalse(operation1Exists);
            assertTrue(operation2Exists);
            assertTrue(operation3Exists);
        }
        {
            // Find paginated
            MetamacCriteria metamacCriteria = new MetamacCriteria();

            MetamacCriteriaDisjunctionRestriction disjunction = new MetamacCriteriaDisjunctionRestriction();
            // By codes of operations created
            MetamacCriteriaPropertyRestriction code1PropertyRestriction = new MetamacCriteriaPropertyRestriction();
            code1PropertyRestriction.setPropertyName(OperationCriteriaPropertyRestriction.CODE.value());
            code1PropertyRestriction.setOperation(OperationType.EQ);
            code1PropertyRestriction.setStringValue(operationDto1.getCode());
            disjunction.setRestrictions(new MetamacCriteriaRestrictionList());
            disjunction.getRestrictions().getRestriction().add(code1PropertyRestriction);

            MetamacCriteriaPropertyRestriction code2PropertyRestriction = new MetamacCriteriaPropertyRestriction();
            code2PropertyRestriction.setPropertyName(OperationCriteriaPropertyRestriction.CODE.value());
            code2PropertyRestriction.setOperation(OperationType.EQ);
            code2PropertyRestriction.setStringValue(operationDto2.getCode());
            disjunction.getRestrictions().getRestriction().add(code2PropertyRestriction);

            MetamacCriteriaPropertyRestriction code3PropertyRestriction = new MetamacCriteriaPropertyRestriction();
            code3PropertyRestriction.setPropertyName(OperationCriteriaPropertyRestriction.CODE.value());
            code3PropertyRestriction.setOperation(OperationType.EQ);
            code3PropertyRestriction.setStringValue(operationDto3.getCode());
            disjunction.getRestrictions().getRestriction().add(code3PropertyRestriction);

            metamacCriteria.setRestriction(disjunction);

            // Pagination
            metamacCriteria.setCountTotalResults(Boolean.TRUE);
            metamacCriteria.setMaximumResultSize(BigInteger.valueOf(1));

            // Find first 1 results
            {
                metamacCriteria.setFirstResult(BigInteger.ZERO);
                FindOperationsResult findOperationsResult = metamacStatisticalOperationsExternalInterfaceV10.findOperations(metamacCriteria);

                // Validate
                assertEquals(BigInteger.valueOf(2), findOperationsResult.getTotalResults());
                assertEquals(BigInteger.valueOf(0), findOperationsResult.getFirstResult());
                assertEquals(BigInteger.valueOf(1), findOperationsResult.getMaximumResultSize());

                assertNotNull(findOperationsResult.getOperations());
                boolean operation1Exists = false;
                boolean operation2Exists = false;
                boolean operation3Exists = false;
                for (OperationBase operationBase : findOperationsResult.getOperations().getOperation()) {
                    if (operationBase.getCode().equals(operationDto1.getCode())) {
                        operation1Exists = true;
                    } else if (operationBase.getCode().equals(operationDto2.getCode())) {
                        operation2Exists = true;
                    } else if (operationBase.getCode().equals(operationDto3.getCode())) {
                        operation3Exists = true;
                    }
                }
                assertFalse(operation1Exists);
                assertTrue(operation2Exists);
                assertFalse(operation3Exists);
            }
            // Find next results (only one)
            {
                metamacCriteria.setFirstResult(BigInteger.valueOf(1));
                FindOperationsResult findOperationsResult = metamacStatisticalOperationsExternalInterfaceV10.findOperations(metamacCriteria);

                // Validate
                assertEquals(BigInteger.valueOf(2), findOperationsResult.getTotalResults());
                assertEquals(BigInteger.valueOf(1), findOperationsResult.getFirstResult());
                assertEquals(BigInteger.valueOf(1), findOperationsResult.getMaximumResultSize());

                assertNotNull(findOperationsResult.getOperations());
                boolean operation1Exists = false;
                boolean operation2Exists = false;
                boolean operation3Exists = false;
                for (OperationBase operationBase : findOperationsResult.getOperations().getOperation()) {
                    if (operationBase.getCode().equals(operationDto1.getCode())) {
                        operation1Exists = true;
                    } else if (operationBase.getCode().equals(operationDto2.getCode())) {
                        operation2Exists = true;
                    } else if (operationBase.getCode().equals(operationDto3.getCode())) {
                        operation3Exists = true;
                    }
                }
                assertFalse(operation1Exists);
                assertFalse(operation2Exists);
                assertTrue(operation3Exists);
            }
        }
    }

    private OperationDto createOperationDto() throws MetamacException {
        OperationDto operationDto = new OperationDto();

        // Auditable information
        operationDto.setCreatedBy("Junit");
        operationDto.setCreatedDate(new Date());

        // Identifier
        operationDto.setCode("PRUEBA01" + RandomStringUtils.random(50, true, true));

        // TITLE
        InternationalStringDto title = new InternationalStringDto();
        LocalisedStringDto title_es = new LocalisedStringDto();
        title_es.setLabel("Título en español de operacion");
        title_es.setLocale("es");
        LocalisedStringDto title_en = new LocalisedStringDto();
        title_en.setLabel("Título en inglés de operacion");
        title_en.setLocale("en");
        title.addText(title_es);
        title.addText(title_en);
        operationDto.setTitle(title);

        // DESCRIPTION
        InternationalStringDto acronym = new InternationalStringDto();
        LocalisedStringDto acronym_es = new LocalisedStringDto();
        acronym_es.setLabel("Descripción en español de operacion");
        acronym_es.setLocale("es");
        LocalisedStringDto acronym_en = new LocalisedStringDto();
        acronym_en.setLabel("Descripción en inglés de operacion");
        acronym_en.setLocale("en");
        acronym.addText(acronym_es);
        acronym.addText(acronym_en);
        operationDto.setAcronym(acronym);

        // RELEASE_CALENDAR
        operationDto.setReleaseCalendar(false);

        // RELEASE_CALENDAR_ACCESS
        operationDto.setReleaseCalendarAccess("http://www.draft.com");

        // SURVEY_TYPE
        operationDto.setSurveyType(statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContextAdministrador(), Long.valueOf(1)));

        // OFFICIALITY_TYPE
        operationDto.setOfficialityType(statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContextAdministrador(), Long.valueOf(1)));

        // SUBJECT_AREA
        ExternalItemBtDto subjectArea = new ExternalItemBtDto();
        subjectArea.setCodeId("PRUEBA");
        subjectArea.setType(TypeExternalArtefactsEnum.CATEGORY);
        subjectArea.setUriInt("uri:external:todo");
        operationDto.setSubjectArea(subjectArea);

        // STATUS
        operationDto.setStatus(StatusEnum.PLANNING);

        // INDICATOR_SYSTEM
        operationDto.setIndicatorSystem(true);

        return operationDto;
    }

    private OperationDto createOperationDtoForInternalPublishing() throws MetamacException {
        OperationDto operationDto = createOperationDto();

        // OBJECTIVE
        InternationalStringDto objective = new InternationalStringDto();
        LocalisedStringDto objective_es = new LocalisedStringDto();
        objective_es.setLabel("OPERACION - OBJECTIVE - ES");
        objective_es.setLocale("es");
        LocalisedStringDto objective_en = new LocalisedStringDto();
        objective_en.setLabel("OPERACION - OBJECTIVE - EN");
        objective_en.setLocale("en");
        objective.addText(objective_es);
        objective.addText(objective_en);
        operationDto.setObjective(objective);

        // SURVEY_TYPE
        operationDto.setSurveyType(statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContextAdministrador(), Long.valueOf(1)));

        // PRODUCER
        ExternalItemBtDto producer01 = new ExternalItemBtDto();
        producer01.setCodeId("ISTAC");
        producer01.setType(TypeExternalArtefactsEnum.AGENCY);
        producer01.setUriInt("uri:interna:todo");
        operationDto.addProducer(producer01);

        ExternalItemBtDto producer02 = new ExternalItemBtDto();
        producer02.setCodeId("INE");
        producer02.setType(TypeExternalArtefactsEnum.AGENCY);
        producer02.setUriInt("uri:interna:todo");
        operationDto.addProducer(producer02);

        // REGIONAL_RESPONSIBLE
        ExternalItemBtDto regionalResponsible01 = new ExternalItemBtDto();
        regionalResponsible01.setCodeId("ISTAC");
        regionalResponsible01.setType(TypeExternalArtefactsEnum.AGENCY);
        regionalResponsible01.setUriInt("uri:interna:todo");
        operationDto.addRegionalResponsible(regionalResponsible01);

        // PUBLISHER
        ExternalItemBtDto publisher01 = new ExternalItemBtDto();
        publisher01.setCodeId("ISTAC");
        publisher01.setType(TypeExternalArtefactsEnum.AGENCY);
        publisher01.setUriInt("uri:interna:todo");
        operationDto.addPublisher(publisher01);

        // COMMON_METADATA
        ExternalItemBtDto commonMetadata = new ExternalItemBtDto();
        commonMetadata.setCodeId("ISTAC");
        commonMetadata.setType(TypeExternalArtefactsEnum.AGENCY);
        commonMetadata.setUriInt("uri:interna:todo");
        operationDto.setCommonMetadata(commonMetadata);

        return operationDto;
    }
}