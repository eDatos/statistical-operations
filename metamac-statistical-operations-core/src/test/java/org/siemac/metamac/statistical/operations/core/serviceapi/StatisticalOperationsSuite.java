package org.siemac.metamac.statistical.operations.core.serviceapi;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDtoFieldsTest;
import org.siemac.metamac.statistical.operations.core.dto.InstanceBaseDtoFieldsTest;
import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDtoFieldsTest;
import org.siemac.metamac.statistical.operations.core.error.StatisticalOperationsCheckTranslationsTest;
import org.siemac.metamac.statistical.operations.core.mapper.ExternalItemsDo2DtoMapperTest;
import org.siemac.metamac.statistical.operations.core.mapper.ExternalItemsDto2DoMapperTest;
import org.siemac.metamac.statistical.operations.core.mapper.InternationalStringsDo2DtoMapperTest;
import org.siemac.metamac.statistical.operations.core.mapper.InternationalStringsDto2DoMapperTest;

/**
 * Spring based transactional test with DbUnit support.
 */

@RunWith(Suite.class)
@Suite.SuiteClasses({StatisticalOperationsCheckTranslationsTest.class,
                        SecurityStatisticalOperationsServiceFacadeTest.class, 
                        StatisticalOperationsBaseServiceTest.class, 
                        StatisticalOperationsListsServiceTest.class,
                        StatisticalOperationsServiceFacadeTest.class,
                        FamilyBaseDtoFieldsTest.class,
                        OperationBaseDtoFieldsTest.class,
                        InstanceBaseDtoFieldsTest.class,
                        ExternalItemsDto2DoMapperTest.class,
                        ExternalItemsDo2DtoMapperTest.class, 
                        InternationalStringsDo2DtoMapperTest.class,
                        InternationalStringsDto2DoMapperTest.class})
public class StatisticalOperationsSuite {
}
